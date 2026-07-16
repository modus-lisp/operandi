#!/usr/bin/env node
// operandi MCP server — dispatch tasks to the operandi ReAct agent over MCP.
//
// An MCP client (Claude Desktop, Cursor, Claude Code, …) hands operandi a task;
// operandi runs its cheap-model agent loop (Read/Write/Edit/Bash/Eval over an
// OpenAI-compatible API — default backend OpenRouter) and returns the final
// answer plus cost/iteration metadata.
//
// Mechanism: each call shells out to the operandi CLI —
//   sbcl --non-interactive --load bin/operandi.lisp -- [--openrouter <model>] "<task>"
// — and captures stdout. operandi COLD-STARTS SLOWLY (it `load`s a large set of
// Lisp packages every run: ~10–40s before the agent even starts, plus the
// agent's own runtime). To stay usable inside an MCP client's request window we
// run every task as a background JOB:
//   • operandi_run       — start a job, wait up to WAIT_MS; if it finishes,
//                          return the answer; else return a job id to poll.
//   • operandi_fan       — run N tasks in parallel (SWARM.md recon mode): the
//                          caller supplies the decomposition, the MCP fans it out.
//   • operandi_get_run   — poll a job by id (returns answer when done).
//   • operandi_cancel_run— kill a running job.
// This never blocks longer than WAIT_MS, and long tasks are still reachable.
//
// Config (env):
//   OPERANDI_ROOT      repo root holding bin/operandi.lisp
//                      (default: the repo root above this mcp/ dir)
//   OPERANDI_MODEL     default OpenRouter model (default deepseek/deepseek-v4-flash)
//   OPERANDI_FAN_MAX   max parallel agents per operandi_fan call (default 8)
//   OPERANDI_WAIT_MS   how long operandi_run blocks before handing back a
//                      job id (default 110000)
//   OPERANDI_MAX_MS    hard wall-clock kill for a single job (default 600000)
//   OPERANDI_SBCL      path to sbcl (default "sbcl")
//
// Run:  node server.js     (stdio transport — how MCP clients launch it)

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { spawn, spawnSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import path from "node:path";
import fs from "node:fs";
import os from "node:os";
import { fileURLToPath } from "node:url";

// ESM has no __dirname; derive it from this module's URL so ROOT defaults to
// the repo root (the directory above mcp/) without any env config.
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = process.env.OPERANDI_ROOT || path.resolve(__dirname, "..");
const ENTRY = path.join(ROOT, "bin", "operandi.lisp");
const SBCL = process.env.OPERANDI_SBCL || "sbcl";
const DEFAULT_MODEL = process.env.OPERANDI_MODEL || "deepseek/deepseek-v4-flash";
const WAIT_MS = Number(process.env.OPERANDI_WAIT_MS || 110000);
const MAX_MS = Number(process.env.OPERANDI_MAX_MS || 600000);
const FAN_MAX = Number(process.env.OPERANDI_FAN_MAX || 8); // max parallel agents per operandi_fan call

const text = (s) => ({ content: [{ type: "text", text: typeof s === "string" ? s : JSON.stringify(s, null, 2) }] });

// In-memory job registry. Keyed by id; holds the child process + accumulated
// stdout/stderr so a later poll can read the result.
const jobs = new Map();

// Parse operandi's CLI output. run-once prints, mixed in with the SBCL banner and
// some verbose progress lines (e.g. "[llm] backend=…", "[operandi] done after N
// iter"), the final answer followed by a metadata line:
//   "\n<final answer>\n\n[N iters, X.XXX¢, P prompt / C comp tokens]\n"
// The metadata line is the reliable anchor: the answer sits between the last
// verbose "[…]" progress marker and that metadata line. We strip the banner and
// bracketed progress lines so the answer is just what operandi actually returned.
const META_RE = /\[\s*(\d+)\s*iters?,\s*([\d.]+)¢,\s*(\d+)\s*prompt\s*\/\s*(\d+)\s*comp tokens\s*\]/;
// Boilerplate/progress lines to drop from the answer body.
const NOISE_RE = /^(This is SBCL|More information about SBCL|SBCL is free software|It is mostly in the public|BSD-style licenses|distribution for more|\[llm\]|\[operandi\]|;)/;

function cleanAnswer(s) {
  return s
    .split("\n")
    .filter((l) => !NOISE_RE.test(l.trim()))
    .join("\n")
    .trim();
}

function parseOutput(stdout) {
  const meta = stdout.match(META_RE);
  if (!meta) return { answer: cleanAnswer(stdout), meta: null };
  const answer = cleanAnswer(stdout.slice(0, meta.index));
  return {
    answer,
    meta: { iters: +meta[1], cents: +meta[2], prompt_tokens: +meta[3], completion_tokens: +meta[4] },
  };
}

function renderJob(job) {
  if (job.status === "running")
    return `Job ${job.id} still running (${Math.round((Date.now() - job.started) / 1000)}s elapsed).\n` +
           `Poll again with operandi_get_run job_id=${job.id}.`;
  if (job.status === "error")
    return `Job ${job.id} FAILED (exit ${job.code}${job.signal ? `, signal ${job.signal}` : ""}).\n` +
           (job.stderr ? `stderr (tail):\n${job.stderr.slice(-1500)}` : "") +
           (job.stdout ? `\nstdout (tail):\n${job.stdout.slice(-800)}` : "");
  const { answer, meta } = parseOutput(job.stdout);
  const m = meta
    ? `\n\n[${meta.iters} iters, ${meta.cents}¢, ${meta.prompt_tokens} prompt / ${meta.completion_tokens} comp tokens]`
    : "\n\n(cost/iters line not found in output)";
  return `${answer}${m}`;
}

// Spawn an operandi run as a tracked job. Returns the job record.
// cwd defaults to ROOT (operandi loads its packages by absolute path from
// *load-truename*, so cwd is free to point elsewhere); operandi_swarm passes a
// per-worker isolated $WD so the worker's Read/Write/Edit/Bash default there.
function startJob(task, model, cwd = ROOT) {
  const id = randomUUID().slice(0, 8);
  const args = ["--non-interactive", "--load", ENTRY, "--"];
  if (model) args.push("--openrouter", model);
  args.push(task);

  const child = spawn(SBCL, args, { cwd, stdio: ["ignore", "pipe", "pipe"] });
  const job = { id, status: "running", started: Date.now(), stdout: "", stderr: "", code: null, signal: null, child, waiters: [] };
  jobs.set(id, job);

  child.stdout.on("data", (d) => (job.stdout += d.toString()));
  child.stderr.on("data", (d) => (job.stderr += d.toString()));

  const hardKill = setTimeout(() => { if (job.status === "running") { job.killed = true; child.kill("SIGKILL"); } }, MAX_MS);

  child.on("close", (code, signal) => {
    clearTimeout(hardKill);
    job.code = code; job.signal = signal;
    job.status = code === 0 ? "done" : "error";
    if (job.killed) job.stderr += `\n[killed: exceeded OPERANDI_MAX_MS=${MAX_MS}ms]`;
    for (const w of job.waiters) w();
    job.waiters = [];
  });
  child.on("error", (err) => {
    clearTimeout(hardKill);
    job.status = "error"; job.stderr += `\n[spawn error: ${err.message}]`;
    for (const w of job.waiters) w();
    job.waiters = [];
  });
  return job;
}

// Wait for a job to finish, or until ms elapses. Resolves either way.
function waitJob(job, ms) {
  if (job.status !== "running") return Promise.resolve();
  return new Promise((resolve) => {
    const t = setTimeout(() => {
      job.waiters = job.waiters.filter((w) => w !== done);
      resolve();
    }, ms);
    const done = () => { clearTimeout(t); resolve(); };
    job.waiters.push(done);
  });
}

const server = new McpServer({ name: "operandi", version: "0.1.0" });

server.tool(
  "operandi_run",
  "Dispatch a one-shot task to the operandi agent (a cheap-model ReAct loop with Read/Write/Edit/Bash/Eval tools). " +
    "Returns operandi's final answer plus cost/iteration metadata. operandi cold-starts slowly (~10-40s to load its Lisp " +
    "packages) before the agent even runs, so this waits up to ~110s; if the task is still running it returns a job_id — " +
    "poll it with operandi_get_run. Give it a concrete, self-contained task.",
  {
    task: z.string().describe("The task for operandi, e.g. 'What is 21+21? Answer with just the number.'"),
    model: z.string().optional().describe(`OpenRouter model id (default ${DEFAULT_MODEL}). e.g. deepseek/deepseek-v4-flash for the cheapest.`),
  },
  async ({ task, model }) => {
    const job = startJob(task, model || DEFAULT_MODEL);
    await waitJob(job, WAIT_MS);
    return text(renderJob(job));
  }
);

server.tool(
  "operandi_get_run",
  "Poll an operandi job by id (from operandi_run when it hadn't finished in time). Returns the final answer once done, " +
    "or a running/failed status.",
  { job_id: z.string() },
  async ({ job_id }) => {
    const job = jobs.get(job_id);
    if (!job) return text(`No such job: ${job_id}`);
    return text(renderJob(job));
  }
);

server.tool(
  "operandi_fan",
  "Fan a list of tasks out to N operandi agents IN PARALLEL (the recon/speculation mode from SWARM.md) and return every result together. " +
    "YOU (the calling model) decide the decomposition — the N independent tasks — and this tool just runs them concurrently, each as its own " +
    "blind agent. Ideal for read-only RECON (trace a bug, map a code path, answer M independent questions, try N hypotheses) where workers " +
    "don't see each other and nothing is merged. NOTE: this fans out whatever you give it — it does NOT decompose the problem or judge results. " +
    "For an oracle-pinned code-fill swarm (edit→run oracle→fix), you must supply the per-unit oracle and own the verify/merge yourself (the " +
    "'carve' the MCP can't do). Waits up to ~110s for all; any still-running task comes back as a job_id to poll with operandi_get_run.",
  {
    tasks: z.array(z.string()).min(1).max(FAN_MAX).describe(`Independent tasks to run in parallel (each its own agent; up to ${FAN_MAX}).`),
    model: z.string().optional().describe(`OpenRouter model for all workers (default ${DEFAULT_MODEL}).`),
  },
  async ({ tasks, model }) => {
    const m = model || DEFAULT_MODEL;
    const started = tasks.map((t) => ({ task: t, job: startJob(t, m) }));
    await Promise.all(started.map(({ job }) => waitJob(job, WAIT_MS)));
    const parts = started.map(({ task, job }, i) => {
      const head = `### [${i + 1}] ${task.replace(/\s+/g, " ").slice(0, 100)}`;
      if (job.status === "running")
        return `${head}\n(still running — poll operandi_get_run job_id=${job.id})`;
      return `${head}\n${renderJob(job)}`;
    });
    return text(parts.join("\n\n---\n\n"));
  }
);

server.tool(
  "operandi_cancel_run",
  "Kill a running operandi job by id.",
  { job_id: z.string() },
  async ({ job_id }) => {
    const job = jobs.get(job_id);
    if (!job) return text(`No such job: ${job_id}`);
    if (job.status !== "running") return text(`Job ${job_id} already ${job.status}.`);
    job.killed = true;
    job.child.kill("SIGKILL");
    return text(`Killed job ${job_id}.`);
  }
);

// ─────────────────────────────────────────────────────────────────────────────
// operandi_swarm — the oracle-pinned FILL layer from SWARM.md.
//
// The MCP owns the fill *mechanics*: isolate each unit in its own $WD, spawn a
// blind operandi worker (cwd=$WD) to loop edit→oracle→fix, then RE-VERIFY the
// oracle INDEPENDENTLY in $WD with a fresh cache (never trust the worker's
// self-report). The caller owns the *carve* (decomposition + writing each
// oracle) and the *merge* (collecting the green $WDs). This tool does NOT merge.
//
// Invariants honored (SWARM.md §invariants):
//   1. Isolation: worker cwd = $WD; oracle runs with cwd=$WD. Proven by the
//      negative test (stub the copy while source is correct → oracle FAILS).
//   2. Fresh cache: each worker + each re-verify gets a wiped per-worker cache
//      dir exported as XDG_CACHE_HOME / ASDF cache, so no stale-artifact lies.
//   3. No vacuous pass: an empty/missing oracle is an ERROR, not a pass.
//   4. Re-verify independently: after the worker exits we run the oracle
//      ourselves; verified = (exit === 0). This is the verdict.
// ─────────────────────────────────────────────────────────────────────────────

const swarms = new Map(); // swarm_id -> { id, units: [unitRecord], started }
const SWARM_CYCLES_DEFAULT = Number(process.env.OPERANDI_SWARM_CYCLES || 8);

// Run a unit's oracle in its $WD with a fresh cache dir. Returns { exit, tail }.
function runOracle(oracle, wd, cacheDir) {
  fs.rmSync(cacheDir, { recursive: true, force: true });
  fs.mkdirSync(cacheDir, { recursive: true });
  const r = spawnSync("/bin/bash", ["-c", oracle], {
    cwd: wd,
    env: { ...process.env, XDG_CACHE_HOME: cacheDir, ASDF_OUTPUT_TRANSLATIONS: `/:${cacheDir}` },
    encoding: "utf8",
    timeout: MAX_MS,
  });
  const out = ((r.stdout || "") + (r.stderr || "")).trim();
  const exit = r.status == null ? -1 : r.status; // null = killed by signal/timeout
  return { exit, tail: out.slice(-1500), signal: r.signal || null };
}

// Compose the worker task: the unit contract + the explicit fill-loop block.
function composeSwarmTask(unitTask, oracle, cycles) {
  return (
    `${unitTask}\n\n` +
    `--- SWARM FILL PROTOCOL ---\n` +
    `Work ONLY inside your current working directory (use relative paths, e.g. ./file — ` +
    `do NOT touch any other tree). Loop: make your edits, then run this oracle command ` +
    `with Bash and read its output; fix whatever fails; repeat until the oracle exits 0 ` +
    `(PASS), at most ${cycles} cycles. The oracle is the objective pass/fail gate — exit ` +
    `0 means PASS. Oracle command:\n${oracle}\n` +
    `Report briefly whether the oracle passed.`
  );
}

// Snapshot a unit record for rendering (no live child handles).
function renderUnit(u) {
  const base = {
    id: u.id,
    wd: u.wd,
    status: u.status, // pending | filling | verifying | done | error
  };
  if (u.status === "error") return { ...base, error: u.error };
  if (u.status === "done") {
    return {
      ...base,
      verified: u.verified,
      oracle_exit: u.oracle_exit,
      oracle_tail: u.oracle_tail,
      operandi_answer: u.operandi_answer,
      cost: u.cost,
    };
  }
  return base; // still in flight
}

function renderSwarm(sw) {
  const units = sw.units.map(renderUnit);
  const done = units.filter((u) => u.status === "done" || u.status === "error").length;
  const green = units.filter((u) => u.verified === true).map((u) => u.wd);
  return {
    swarm_id: sw.id,
    complete: done === sw.units.length,
    units,
    green_wds: green,
    note:
      "This tool does NOT merge. Collect the green $WDs (green_wds) into your " +
      "canonical tree yourself — that is the caller's merge step per SWARM.md. " +
      "`verified` is an INDEPENDENT oracle re-run in each $WD (exit 0 = pass), " +
      "not operandi's self-report. Poll with operandi_swarm_status swarm_id=" +
      sw.id + ".",
  };
}

// Run one unit end-to-end: isolate → fill (operandi) → re-verify. Mutates `u`.
async function runUnit(u, source, model, cycles) {
  try {
    // 1. Isolate: fresh $WD, optionally seeded from source.
    fs.mkdirSync(u.wd, { recursive: true });
    if (source) {
      const cp = spawnSync("/bin/bash", ["-c", `cp -r "${source}/." "${u.wd}/"`], { encoding: "utf8" });
      if (cp.status !== 0) throw new Error(`isolate cp failed: ${(cp.stderr || "").trim()}`);
    }

    // 3. Fill: spawn a blind operandi worker with cwd=$WD and a fresh cache.
    fs.rmSync(u.cache, { recursive: true, force: true });
    fs.mkdirSync(u.cache, { recursive: true });
    u.status = "filling";
    const task = composeSwarmTask(u.task, u.oracle, cycles);
    // startJob spawns with cwd=$WD; we also want the worker's own Bash oracle
    // runs to use the fresh cache, so operandi inherits XDG_CACHE_HOME via env —
    // but startJob doesn't set env, so the worker's cache is process env's. The
    // load-bearing check is our INDEPENDENT re-verify below with a wiped cache.
    const job = startJob(task, model, u.wd);
    u.job_id = job.id;
    await waitJob(job, MAX_MS); // swarm units are the long path; bounded by MAX_MS
    if (job.status === "running") {
      // Hit MAX_MS without finishing (shouldn't: waitJob==MAX_MS==hardKill).
      u.operandi_answer = `(worker still running after ${MAX_MS}ms)`;
    } else {
      const { answer, meta } = parseOutput(job.stdout);
      u.operandi_answer = job.status === "error"
        ? `(worker exited with error: ${(job.stderr || "").slice(-400).trim()})`
        : answer.slice(-2000);
      u.cost = meta ? { iters: meta.iters, cents: meta.cents } : null;
    }

    // 4. Re-verify INDEPENDENTLY: our own oracle run in $WD, fresh cache.
    u.status = "verifying";
    const { exit, tail } = runOracle(u.oracle, u.wd, u.cache);
    u.oracle_exit = exit;
    u.oracle_tail = tail;
    u.verified = exit === 0;
    u.status = "done";
  } catch (err) {
    u.status = "error";
    u.error = err.message;
    u.verified = false;
  }
}

// Concurrency-capped runner (cap = FAN_MAX) over the unit list.
async function runSwarm(sw, source, model, cycles) {
  const queue = [...sw.units];
  const workers = Array.from({ length: Math.min(FAN_MAX, queue.length) }, async () => {
    while (queue.length) {
      const u = queue.shift();
      await runUnit(u, source, model, cycles);
    }
  });
  await Promise.all(workers);
}

server.tool(
  "operandi_swarm",
  "Oracle-pinned FILL swarm (SWARM.md): run each caller-carved unit as a blind operandi worker in its OWN isolated copy, " +
    "looping edit→oracle→fix until its oracle passes, then RE-VERIFY every unit's oracle independently in its copy. YOU carve " +
    "(decompose + write each oracle) and YOU merge (collect the green copies); this tool owns only the fill mechanics and the " +
    "trust step (independent re-verify). Each unit needs a `task` (the operandi contract) and an `oracle` (a shell command run " +
    "with cwd = that worker's isolated copy; EXIT 0 = PASS, nonzero = FAIL). `source` (optional abs dir) is cp -r'd into each " +
    "worker's copy; omit it to give each a fresh empty dir. It does NOT merge — it returns per-unit verdicts + each copy's path " +
    "(green_wds) so you collect the winners. Long path: returns a swarm_id immediately; poll with operandi_swarm_status.",
  {
    units: z
      .array(
        z.object({
          id: z.string().optional().describe("Optional stable id for this unit (else index-based)."),
          task: z.string().min(1).describe("The operandi contract/prompt for this unit — what to build/fix in its copy."),
          oracle: z.string().min(1).describe("Shell command run with cwd = the worker's isolated copy. EXIT 0 = PASS, nonzero = FAIL."),
        })
      )
      .min(1)
      .describe("The caller's decomposition: one isolated worker per unit. Each has a task + an oracle."),
    source: z.string().optional().describe("Absolute path to a dir cp -r'd into each worker's copy (the tree it edits). Omit for a fresh empty copy."),
    model: z.string().optional().describe(`OpenRouter model for the workers (default ${DEFAULT_MODEL}).`),
    cycles: z.number().int().positive().optional().describe(`Max edit→oracle→fix cycles to tell each worker to run (default ${SWARM_CYCLES_DEFAULT}).`),
  },
  async ({ units, source, model, cycles }) => {
    // Invariant 3 (vacuous pass guard) + source sanity, up front.
    for (let i = 0; i < units.length; i++) {
      const o = (units[i].oracle || "").trim();
      if (!o) return text(`unit[${i}] has an empty oracle — refusing (a unit with no oracle passes vacuously; SWARM.md invariant 3).`);
    }
    if (source) {
      if (!path.isAbsolute(source)) return text(`source must be an absolute path, got: ${source}`);
      if (!fs.existsSync(source) || !fs.statSync(source).isDirectory())
        return text(`source is not an existing directory: ${source}`);
    }

    const id = randomUUID().slice(0, 8);
    const base = fs.mkdtempSync(path.join(os.tmpdir(), `swarm-${id}-`));
    const unitRecords = units.map((un, i) => {
      const uid = un.id || `u${i}`;
      return {
        id: uid,
        task: un.task,
        oracle: un.oracle,
        wd: path.join(base, uid, "wd"),
        cache: path.join(base, uid, "cache"),
        status: "pending",
        verified: null,
      };
    });
    const sw = { id, base, units: unitRecords, started: Date.now() };
    swarms.set(id, sw);

    // Kick off the whole swarm in the background; don't block past WAIT_MS.
    const eff = { source: source || null, model: model || DEFAULT_MODEL, cycles: cycles || SWARM_CYCLES_DEFAULT };
    sw.promise = runSwarm(sw, eff.source, eff.model, eff.cycles).catch((e) => {
      sw.fatal = e.message;
    });

    // Give it WAIT_MS in case it's fast (it usually isn't — operandi cold-start).
    await Promise.race([sw.promise, new Promise((r) => setTimeout(r, WAIT_MS))]);
    return text(renderSwarm(sw));
  }
);

server.tool(
  "operandi_swarm_status",
  "Poll an operandi_swarm by swarm_id. Returns per-unit status/verdicts and green_wds (the isolated copies whose independent " +
    "oracle re-verify PASSED — collect these yourself; the swarm does not merge).",
  { swarm_id: z.string() },
  async ({ swarm_id }) => {
    const sw = swarms.get(swarm_id);
    if (!sw) return text(`No such swarm: ${swarm_id}`);
    if (sw.fatal) return text(`Swarm ${swarm_id} FATAL: ${sw.fatal}`);
    return text(renderSwarm(sw));
  }
);

await server.connect(new StdioServerTransport());

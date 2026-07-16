# operandi-mcp

An MCP server that exposes the **operandi** agent — a cheap-model ReAct loop
(Read/Write/Edit/Bash/Eval over an OpenAI-compatible API, default backend
OpenRouter) — as dispatchable MCP tools. Hand it a task from any MCP client
(Claude Desktop, Cursor, Claude Code, …) and it runs operandi's agent loop and
returns the final answer plus cost/iteration metadata.

## Tools

- **`operandi_run { task: string, model?: string }`** — dispatch a one-shot task.
  Returns operandi's final answer + a `[N iters, X¢, prompt/comp tokens]` line.
  operandi cold-starts slowly (it `load`s a large set of Lisp packages every
  run — ~10–40s before the agent even starts, plus the agent's own runtime), so
  this waits up to `OPERANDI_WAIT_MS` (default 110s). If the task is still
  running when that elapses, it returns a `job_id` — poll it with
  `operandi_get_run`. `model` overrides the OpenRouter model (default
  `deepseek/deepseek-v4-flash` — ds4f, the cheap swarm workhorse).
- **`operandi_fan { tasks: string[], model?: string }`** — run up to
  `OPERANDI_FAN_MAX` (default 8) tasks **in parallel**, each its own operandi
  agent, and return every result together. This is SWARM.md's **recon /
  speculation** mode: the *caller* decomposes the problem into N independent
  tasks; the MCP just fans them out (blind workers, nothing merged). Great for
  read-only recon (trace a bug, map a path, answer M questions, try N
  hypotheses). It does **not** decompose or judge — for an oracle-pinned code
  *fill* (edit→run oracle→fix) the caller must supply the oracle and own the
  verify/merge (the "carve" the MCP can't do). See [Fan-out](#fan-out-vs-carve).
- **`operandi_swarm { units: [{ id?, task, oracle }], source?, model?, cycles? }`**
  — the **oracle-pinned FILL layer** from SWARM.md. Runs each caller-carved unit
  as a *blind* operandi worker in its **own isolated copy** (`$WD`), looping
  `edit → run oracle → fix` until the oracle passes, then **re-verifies every
  unit's oracle independently** in its copy. The MCP owns only the fill
  *mechanics* + the trust step; **you** carve (decompose + write each oracle) and
  **you** merge (collect the green copies). See
  [The fill contract](#the-fill-contract) below. It does **not** merge. Long
  path: returns a `swarm_id` immediately (bounded by `OPERANDI_WAIT_MS`); poll
  with `operandi_swarm_status`.
- **`operandi_swarm_status { swarm_id: string }`** — poll a swarm. Returns
  per-unit `{ id, wd, verified, oracle_exit, oracle_tail, operandi_answer, cost }`
  plus `green_wds` (the `$WD`s whose independent oracle re-verify PASSED —
  collect these yourself).
- **`operandi_get_run { job_id: string }`** — poll a job started by
  `operandi_run`/`operandi_fan` that hadn't finished in time.
- **`operandi_cancel_run { job_id: string }`** — kill a running job.

### The fill contract

`operandi_swarm` implements SWARM.md's *fill* stage. The division of labor:

- **You (the caller) carve.** Decompose the problem into independent `units`.
  For each unit you write a `task` (the operandi contract — what to build/fix)
  and an `oracle`: **a shell command that objectively says pass/fail. Exit 0 =
  PASS, nonzero = FAIL.** The oracle runs with **cwd = that worker's isolated
  copy** (`$WD`), so write it in terms of relative paths (e.g.
  `grep -qx RIGHT answer.txt`, `make test`, a Lisp load-and-run one-liner).
- **The MCP fills.** For each unit, in parallel (capped at `OPERANDI_FAN_MAX`):
  1. **Isolate** — a fresh `$WD` under `os.tmpdir()`; if `source` (an absolute
     dir) is given it's `cp -r`'d in, else `$WD` starts empty. The worker is
     spawned with **cwd = `$WD`**, so its Read/Write/Edit/Bash default there — it
     edits only its copy, never your canonical tree.
  2. **Fresh cache** — a per-worker cache dir, wiped before the re-verify
     (`XDG_CACHE_HOME`/ASDF output), so no stale artifact yields a false pass.
  3. **Fill** — a blind operandi worker loops `edit → run oracle → fix` up to
     `cycles` times (default 8).
  4. **Re-verify independently** — after the worker exits, the MCP runs the
     oracle *itself* in `$WD` with a clean cache. `verified = (exit === 0)`. This
     is the verdict — operandi's self-report is never trusted.
- **You merge.** The tool returns per-unit verdicts + each copy's path and a
  `green_wds` list. Collecting the green copies into your canonical tree is
  **your** merge step — the swarm never merges.

Invariants honored (each cost a wasted wave to learn — see SWARM.md): **(1)**
isolation is load-bearing — oracle cwd = `$WD`, proven by the negative test
(stub a copy while `source` is correct → oracle FAILS); **(2)** fresh cache per
worker/re-verify; **(3)** an empty/missing oracle is an **error**, not a vacuous
pass; **(4)** the independent re-verify is the trust step, always run.

### Fan-out vs carve

The MCP can own the **mechanics** of fanning out (spawn N parallel agents,
collect results) — that's `operandi_fan`. It cannot own the **judgment**: what
the N tasks *are* (decomposition), and for code fills the **oracle** (an
objective pass/fail command per unit) plus the merge decision. Per SWARM.md
those are the calling model's "carve + merge" job. So: recon → just list your N
tasks and call `operandi_fan`; oracle-pinned fill → the caller carves + verifies,
optionally driving `operandi_fan`/`operandi_run` for the fill loop.

### Why the job model

operandi is invoked by shelling out to its CLI:

    sbcl --non-interactive --load bin/operandi.lisp -- [--openrouter <model>] "<task>"

capturing stdout, and parsing off the final answer + the `[…iters…]` metadata
line. Because a cold start alone can eat much of a typical MCP request window,
every run is tracked as a **background job**: `operandi_run` starts it and blocks
only up to `OPERANDI_WAIT_MS`, so the tool never hangs past that bound, and long
tasks stay reachable via `operandi_get_run`. A hard `OPERANDI_MAX_MS` (default
10min) kills runaway jobs.

## Config (env)

| Var                | Default                 | Meaning                                              |
|--------------------|-------------------------|------------------------------------------------------|
| `OPERANDI_ROOT`    | dir above `mcp/`        | repo root holding `bin/operandi.lisp`                |
| `OPERANDI_MODEL`   | `deepseek/deepseek-v4-flash` | default OpenRouter model (ds4f)                             |
| `OPERANDI_WAIT_MS` | `110000`                | how long `operandi_run` blocks before handing a job id |
| `OPERANDI_MAX_MS`  | `600000`                | hard wall-clock kill for a single job                |
| `OPERANDI_FAN_MAX`  | `8`                     | max parallel agents per `operandi_fan` call (also the `operandi_swarm` concurrency cap) |
| `OPERANDI_SWARM_CYCLES` | `8`                 | default max edit→oracle→fix cycles per swarm unit    |
| `OPERANDI_SBCL`    | `sbcl`                  | path to the `sbcl` binary                            |

The OpenRouter API key is read by operandi itself from
`~/.operandi/openrouter.token` (not by this server).

## MCP client config

```json
{
  "mcpServers": {
    "operandi": {
      "command": "node",
      "args": ["/path/to/operandi/mcp/server.js"],
      "env": {
        "OPERANDI_MODEL": "deepseek/deepseek-v4-flash"
      }
    }
  }
}
```

## Test it by hand

```sh
# tools/list over stdio:
printf '%s\n' \
 '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"c","version":"0"}}}' \
 '{"jsonrpc":"2.0","method":"notifications/initialized"}' \
 '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' | node server.js
```

## Notes

- Run `npm install` in this directory to fetch the dependencies
  (`@modelcontextprotocol/sdk` + `zod`).
- `OPERANDI_ROOT` defaults to the repo root (the directory above `mcp/`),
  derived from the server's own location — so it works from a clone without
  any env config.

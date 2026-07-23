# operandi swarm queue — design spec

Status: **proposal / pick-up-ready**. Author: ynniv + Claude. Date: 2026-07-22.

## 1. Why

We run **swarms**: many operandi agents, each on a cheap model, each filling one
small isolated unit of a large mechanical job (implement a spec surface, fix a
class of bugs, port N call-sites), gated by an **objective oracle** so we keep
only what verifiably works. The pattern is proven — see `combat/SWARM.md` and the
reference implementation in `weft/tools/swarm/forms-*` (a 6-unit and then a
12-job wave that landed real WPT test passes).

Today the runner is `xargs -P <pool>` over a fixed job list. That is fine for a
dozen jobs in one sitting. It does **not** scale to what we actually want:

> **A hundred (or more) workers, running unattended, that I never have to babysit.**
> Tokens cost the same today or tomorrow. *Serial work costs my time; parallel
> work does not.* Optimize for wall-clock and for zero human-in-the-loop, not for
> token thrift.

So the goal is a **persistent, resumable, observable work-queue** that turns "a
swarm" from a one-shot shell command into a standing service you enqueue against
and walk away from.

### Design values (inherited, non-negotiable)
- **Local over containers.** Plain processes on our own hosts. No Docker/k8s.
  Coordination via the filesystem (and optionally a tiny TCP broker), not a cloud
  queue. (See the operator's "local over containers" preference.)
- **Isolation is sacred.** Every worker edits only its own repo copy, loads via
  `CL_SOURCE_REGISTRY (:tree $WD) :ignore-inherited-configuration`, wipes its own
  fasl cache. A worker must be *unable* to affect canonical or another worker.
  This is already proven (a corrupted copy aborts compilation with no result
  line — it cannot fall through to the canonical tree).
- **Oracle-gated, never vacuous.** A job defines its own success predicate that
  cannot pass without doing the work. Merge only re-verifies in canonical and
  **keeps-if-improves**.
- **Cheap fan + strong gate.** Most workers on a cheap model; a minority of
  variants on a stronger model as the "good gate." (See "cheap-model scope.")

## 2. What exists to build on (don't re-invent)

The single-wave primitives already work and should be *extracted*, not rewritten:

| Primitive | Where | Keep as |
|---|---|---|
| Per-unit repo copy + fasl wipe | `forms-wave.sh` | `provision(job) -> workdir` |
| Isolated registry / oracle contract | `forms-worker.sh` | `oracle(job) -> {passed, failed}` |
| operandi agent loop (Read/Write/Edit/Bash/Eval) | `bin/operandi.lisp` | `agent(job)` step |
| Diversified variants + keep-best | `forms-wave2.sh` | `plan()` + `reduce()` |
| Re-verify-in-canonical / keep-if-improves | `forms-merge.sh` | `merge(result)` |
| Injection-point carve (disjoint self-registering files) | weft `*element-proto-extensions*` | the pattern jobs should target |

The queue is the missing **control plane** around these.

## 3. Job model

A **job** is the atom of work. It is a directory (self-describing, portable):

```
jobs/<job-id>/
  manifest.json      # everything the worker + merger need (below)
  task.md            # the natural-language brief handed to the agent
  status             # pending | claimed | running | done | failed | merged | abandoned
  lease.json         # {host, pid, heartbeat_ts, deadline_ts}  (present while claimed)
  result.json        # {passed, failed, oracle_line, artifact_path, tokens, cost_cents, iters}
  log                # full agent stdout/stderr
  workdir/           # the isolated repo copy (or a pointer to it; see §6 disk)
```

`manifest.json`:
```json
{
  "id": "weft.forms.valueasnumber-a",
  "group": "weft.forms.valueasnumber",   // sibling variants of the same unit
  "repo": "/home/claude/weft",           // canonical source to copy
  "edit_glob": "src/script/forms-valueasnumber.lisp",  // the ONLY file(s) the agent may touch
  "oracle": "cd $WD && ... forms-oracle.lisp ... (run \"valueasnumber\")",
  "success": {"metric": "failed", "goal": 0, "direction": "min"},
  "model": "deepseek/deepseek-v4-flash",
  "budget": {"max_iters": 80, "max_cost_cents": 50, "timeout_s": 1500},
  "merge": {"target": "src/script/forms-valueasnumber.lisp",
            "strategy": "keep-best-of-group | keep-if-improves"},
  "deps": [],                             // job-ids that must be `merged` first
  "attempt": 1, "max_attempts": 2
}
```

Design notes:
- **`edit_glob` is enforced**, not trusted: the provisioner can make everything
  else read-only, or the merger diffs the workdir against the source and rejects
  any job that touched a file outside `edit_glob` (defense in depth vs. a
  worker that wanders — cf. the worktree-symlink hazard note).
- **`group`** ties variants together so `reduce()` can keep-best across a group.
- **`success`** is a machine-checkable predicate parsed from the oracle's tally
  line (`UNIT x: P passed, F failed`). Generalize the tally format so any oracle
  (CSS parser vectors, WPT subtests, test262 slices) emits `PASS=<n> FAIL=<n>`.

## 4. Components

Four small long-lived roles. Each is a plain process; any can run on any host.

### 4.1 `enqueue` (planner / dispatcher)
- Takes a **plan**: a repo, a carve (list of units), a variant policy (e.g.
  `a=narrow/cheap, b=extend/cheap, c=full/strong`), budgets. Emits N job dirs
  into `jobs/` with `status=pending`.
- Can be re-run to add jobs (loop-until-dry: enqueue a fresh round while any
  group still has failing oracles and the round budget isn't spent).
- Pure function of (plan, current results) → new jobs. No side effects beyond
  writing job dirs.

### 4.2 `worker` (the scaling unit — run 100 of these)
Daemon loop:
```
loop:
  job = claim()                      # atomic; see §5
  if none: sleep(jitter); continue
  provision(job)                     # cp repo -> workdir, wipe fasls, chmod
  heartbeat_start(job)               # background: touch lease every T seconds
  run agent(job) with budget         # bin/operandi.lisp --openrouter <model> "<task>"
  {passed, failed} = oracle(job)     # isolated registry, wiped cache
  write result.json (+ tokens/cost from operandi's usage summary)
  status = done (oracle ran) | failed (crash/timeout/over-budget)
  release(job)
```
- Stateless beyond the job dir. Kill -9 at any point → the lease expires → the
  job is reclaimable. **A dead worker never wedges a job.**
- Concurrency = number of worker processes. Start them with a supervisor
  (systemd-less: a `spawn N` script + a respawn loop; note the SBCL
  `--disable-debugger` gotcha — a background thread's unhandled condition kills
  the process, so wrap the loop in `handler-case serious-condition` and let the
  supervisor respawn).
- Per-host worker count is bounded by **RAM** (each oracle load is a fresh SBCL,
  ~0.5–1 GB peak) and **cores**, not by the queue. The queue just hands out work.

### 4.3 `reap` (supervisor / lease manager)
- Scans `running/` leases; any whose `heartbeat_ts` is older than `2×interval`
  or past `deadline_ts` is **requeued** (`status=pending`, `attempt++`) up to
  `max_attempts`, else `abandoned`.
- Enforces global caps the workers can't see individually: total concurrency,
  **per-model rate limits / spend ceiling** (stop claiming new jobs when the
  running token budget for the day is exhausted — mirror the "+500k" budget
  ceiling idea).

### 4.4 `merge` (the only thing that touches canonical — single-writer)
- Watches for `status=done`. For a group with `keep-best-of-group`, waits until
  the group is complete (or a quorum), picks the lowest-`failed` variant.
- **Re-verifies in canonical**: copy the winning file(s) into the real repo,
  run the oracle against canonical, keep only if `failed` did not regress; else
  revert. (Exactly `forms-merge.sh`, generalized.)
- Commits kept results (one commit per group, oracle deltas in the message),
  using the repo's convention. Runs **serially** — canonical has one writer, so
  merges never race even with 100 workers.
- Because jobs target **disjoint injection points** (the self-registering-file
  pattern), merges are conflict-free by construction. If a plan can't be carved
  into disjoint files, that's a planning bug, not a merge problem.

## 5. Coordination substrate

Two options; **start with A**, design so B is a drop-in.

**A. Filesystem queue (default, zero-dep, local-first).**
- Claim = **atomic rename**: `mv jobs/<id> running/<host>-<pid>/<id>` (POSIX
  rename is atomic within a filesystem; the winner is whoever's rename
  succeeds — everyone else gets ENOENT and moves on). No lockfiles, no daemon.
- Works across hosts over a shared FS (NFS/9p) *if* rename is atomic there;
  otherwise use the `O_CREAT|O_EXCL` lease-file trick per job.
- State is just directories: `jobs/ running/ done/ failed/ merged/`. `ls` is your
  dashboard. `grep` is your query engine. Survives reboots and session ends.

**B. Tiny TCP broker (when multi-host FS coordination gets painful).**
- A single-file SBCL/`conch`-served broker exposing `claim/heartbeat/complete`
  over localhost + LAN. Same job model; the job *dir* still holds the payload,
  the broker only arbitrates claims. Reuse `natrium`/`conch` — no redis, no
  external broker, consistent with local-over-containers.

Do **not** reach for Docker, k8s, Celery, or a cloud queue. The whole point is
that this runs on our boxes with tools we already own.

## 6. Disk at 100 workers (the real scaling wall)

100 × a full repo copy is the actual constraint, not CPU or the queue.
`weft` ≈ 105 MB → 100 copies ≈ **10 GB** *if copied naively*, plus fasl caches.
Options, cheapest first:

1. **`cp --reflink=auto`** on a CoW filesystem (btrfs/xfs) — copies are ~free
   until written; a worker only diverges by the one file it edits. **Preferred.**
2. **`git worktree`** per job off a shared object store — but heed the
   worktree-symlink hazard (a worker may symlink a gitignored data dir and a
   later merge clobbers the parent): forbid symlinks, give absolute paths,
   `git show --stat` before any merge.
3. **overlayfs** — shared read-only lower (canonical) + per-worker upper. Fast,
   but needs mount privileges; keep as a fallback.
4. Shared **read-only base** + only the `edit_glob` files copied writable, with a
   registry that layers the writable file over the base tree. Most complex; only
   if 1–3 are unavailable.

Also: cap total fasl-cache disk (one `XDG_CACHE_HOME` per worker, wiped between
jobs), and GC `done/merged` workdirs on a retention timer (keep `result.json` +
`log`, drop `workdir/`).

## 7. Observability & cost

- **`status` command**: counts by state, per-group best-so-far, live worker
  count, oracle-pass trend, **cumulative tokens + cost** (operandi already prints
  `[N iters, C¢, prompt/completion tok]` — capture it into `result.json`).
- One **log per job**; a `tail`-able global event stream (each state transition
  is one line) suitable for the Monitor pattern (emit on `done|failed|merged`).
- **Budget ceiling**: a per-day / per-plan spend cap in `reap`; when hit, stop
  claiming (running jobs finish). Report what was left un-run — never silently
  truncate coverage.

## 8. Failure semantics (must-haves)

- Worker dies → lease expires → job requeued (bounded attempts). No wedge.
- Oracle can't pass (feature genuinely beyond the cheap model) → job `done` with
  `failed>0`; `reduce()` keeps the best variant; planner may escalate the group
  to a stronger model on the next round (auto-tiering).
- Merge would regress canonical → revert, mark the group `needs-human`, keep the
  artifact for inspection. Canonical is never left broken.
- Everything is **resumable**: relaunching `worker`/`reap`/`merge` after a crash
  picks up from the on-disk state. No in-memory-only progress.

## 9. Interfaces (target CLI)

```
operandi-swarm enqueue --plan plan.json          # write jobs/
operandi-swarm worker  --host $(hostname) [--models flash,pro] [--max-ram 0.7]
operandi-swarm reap    [--lease-timeout 300 --budget-cents 20000]
operandi-swarm merge   --repo /home/claude/weft [--commit]
operandi-swarm status  [--group weft.forms.*] [--watch]
operandi-swarm spawn   --workers 100             # convenience: fork N supervised workers
```
`plan.json` is the carve + variant policy; `enqueue` expands it into jobs. Keep
each verb a small script/Lisp entrypoint; the job dir is the ABI between them, so
verbs can be reimplemented independently.

## 10. Milestones

1. **Extract** `provision/oracle/agent/merge` from `weft/tools/swarm/forms-*`
   into reusable operandi entrypoints; keep the forms wave working through them
   (regression guard).
2. **Filesystem queue + worker daemon + reap** (§4.2–4.3, substrate A). Re-run
   the 12-job forms wave through the queue; identical results, now crash-safe.
3. **`spawn 100` + disk strategy** (§6 option 1). Prove 100 workers on one host
   against a large carve (e.g. a test262 or WPT subtree) without OOM/disk blowup.
4. **Planner auto-tiering + loop-until-dry** (§4.1, §8). Enqueue rounds until
   groups converge or budget caps out.
5. **`status --watch` dashboard + cost accounting** (§7).
6. **(Optional) TCP broker** for multi-host (substrate B), reusing conch.

## 11. Open decisions for the implementer

- CoW filesystem availability on the target hosts (picks the §6 strategy).
- Single-host-100 vs. multi-host-fan first? (Multi-host needs substrate B sooner.)
- Merge cadence: per-group-complete vs. streaming keep-if-improves.
- How much planning is static (carve up front) vs. reactive (planner consumes
  results and emits follow-ups) — start static, add reactive in milestone 4.
- Where canonical commits land (direct-to-branch vs. an integration branch the
  human reviews) — default to a per-plan integration branch once volume is high.

---
*Prototype to read first:* `weft/tools/swarm/forms-{wave,worker,merge}.sh`,
`weft/tools/swarm/forms-wave2.sh` (variants + keep-best), and
`weft/inspect/forms-oracle.lisp` (an oracle emitting the tally line). The queue
generalizes exactly these into a standing service. `combat/SWARM.md` has the
operating invariants every job must still honor.

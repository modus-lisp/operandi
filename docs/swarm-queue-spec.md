# operandi swarm — design spec

Status: **proposal / pick-up-ready** (v2 — the agenda model). Authors: ynniv +
Claude. Date: 2026-07-23. Supersedes the v1 "control-plane / factory-line" framing.

## 0. The frame: a plan that's *grown, not authored*

We are not building a factory line — a fixed sequence of stations that work flows
through. We are building **a todo list that a variable number of agents work on**.

The list starts as **one item: the whole job**. A worker that picks it up rarely
finishes it; it looks at the item and **grows** it — splits it into sub-items and
puts them back on the list. Other workers pick those up, grow or close them. The
list **breathes**: it expands as decomposition outruns completion, then contracts
as leaves close and roll up, until the root item closes. **The tree of work is
discovered by doing it, not decreed up front.**

This is old, load-bearing prior art, not a novelty:
- **Work-stealing pools** (Cilk, ForkJoinPool, rayon): a shared task pool,
  fungible workers, tasks that spawn subtasks. That *is* "a variable number of
  people on a todo list."
- **Blackboard architecture** (Hearsay-II): a shared blackboard, opportunistic
  workers contributing wherever they see an opening — **no central controller**;
  coordination emerges from the shared state. There is no orchestrator to be a
  bottleneck or a single point of failure. The list coordinates.
- **Goal-reduction / agenda search** (means-ends, HTN, Prolog): pop a goal; solve
  it or reduce it to sub-goals; the agenda breathes.

Two consequences fall out, and they are the whole reason to build it this way:

1. **A single agent and a hundred are the same program at different pool sizes.**
   One worker recursively grows-and-closes the tree serially — that is just a
   normal agent. Add workers and the identical list runs in parallel. There is no
   separate "swarm mode." In fact this is **Claude Code turned inside-out**: Claude
   Code already keeps a todo list it grows and shrinks (`TodoWrite`) and delegates
   sub-items to subagents. We externalize that list to durable disk and turn the
   subagents into a fungible pool that pulls from it. Same shape — bigger, and
   standing.

2. **The rigid parts go only at the irreversibility boundaries.** Claude Code's
   own lesson (from its docs): *expect the model to be uncertain; make reversibility
   and inspection deterministic.* Its traditional code isn't a pipeline — it's thin
   membranes placed exactly where a wrong move can't be undone: read-before-edit,
   write-requires-read, protected paths never auto-approved, a checkpoint before
   every prompt with rewind, bounded/backgrounded output. Everything *between* those
   membranes — which tool, what order, when done, how to decompose — is the model.
   We copy that placement exactly (§3).

So: **not a factory line, not a wild-LLM party.** A model conductor — distributed
across a fungible pool with no central orchestrator — running on a substrate that
makes being wrong cheap and undoable.

## 1. Why (the motivation under the frame)

> A hundred (or more) workers, running unattended, that I never have to babysit.
> Tokens cost the same today or tomorrow. **Serial work costs my time; parallel
> work does not.** Optimize for wall-clock and zero-human-in-the-loop, not token
> thrift.

Today the runner is `xargs -P` over a fixed job list — fine for a dozen jobs in one
sitting, but it can't be a standing service you seed and walk away from, and its job
list is authored up front. The agenda model removes the up-front authoring: you seed
one item and the workers grow the rest.

### Design values (inherited, non-negotiable)
- **Grown, not authored.** No up-front carve is required; decomposition is *work*,
  done by workers, at runtime. (A carve *may* be seeded as a hint — see §8 dedup.)
- **Determinism only at irreversibility boundaries.** Everything else is the model.
- **Models over harness rules — in the judgment layer.** How to split, when an
  item is small enough to just do, whether a review passes where no oracle exists,
  when to escalate — model judgment; static rules rot. Correctness gates — claim,
  verify, merge — stay deterministic (§3, §9).
- **Local over containers.** Plain processes on our own hosts; coordination via the
  filesystem (optionally a tiny TCP broker). No Docker/k8s/cloud queue.
- **Isolation is sacred.** A worker can affect *only* its own copy and its own
  edit-scope — never canonical, never another worker's. Already proven: a corrupted
  copy aborts compilation with no result line; it cannot fall through to canonical.
- **Cheap fan + strong gate.** Most workers on a cheap model; a minority of variants
  on a stronger model as the "good gate."

## 2. The model: items that *close* or *grow*

An **item** is a **goal**, not a pipeline stage. A worker that claims an item does
exactly one of two things:

- **Close it** — the item is small enough to just do. Carry it out, then **verify**
  (§3). A closed leaf that passes its oracle is `verified`; it may then be `merged`.
- **Grow it** — the item is too big or too vague to do in one shot. Split it into
  child items, put them on the list, and mark this item a **parent** that is `done`
  only when its children are. Splitting *is the work* here — a model act.

Items form a **tree with dependency edges** (§8): most children of a split are
independent (parallel), some are ordered (B needs A). An item is **ready** to claim
only when its dependencies are `verified`.

**Roles are emergent, not fixed stages.** "Implement this surface", "review this
diff", "merge this group", "figure out how to carve X", "escalate this to a stronger
model" are all just items — the model, plus the item's shape, decides what kind of
work an item is. There is no `code → review → merge` assembly line; there is a list,
and workers that close or grow whatever they pull.

## 3. The membranes: where the traditional code lives

Per §0.2, the deterministic code sits *only* at the list's irreversibility
boundaries. These are the fixed substrate; everything else is grown.

| Membrane | What it guarantees | CC analog |
|---|---|---|
| **Atomic claim** | two workers can never own one item (POSIX `rename`, §6) | tool exec succeeds-or-errors, no negotiation |
| **Isolation** | a worker touches only its `$WD` + `edit_scope`; canonical is unreachable | read-before-edit; protected paths |
| **Verify** | a leaf is `verified` **iff its oracle passes** — a model cannot *declare* done | tests as the truth, not model self-report |
| **Merge** | single-writer to canonical; re-verify-in-canonical, **keep-if-improves**, **reject if it touched outside `edit_scope`** | checkpoint + rewind (reversible); protected paths |
| **Journal** | the list is durable; a killed worker's item is re-claimable; re-attempt is safe | sessions as JSONL, deterministic resume |
| **Budget / depth caps** | the breathing must *contract* — bound runaway decomposition and spend | bounded/backgrounded output; timeouts |

The single hard rule: **a model's word is never load-bearing at an irreversibility
boundary.** "I merged it" is not a proof; the oracle re-run in canonical is. This is
the exact seam Claude Code draws, and it is what makes 100 model-driven workers safe
against a canonical tree they can't corrupt.

## 4. The item, concretely (the ABI)

An item is a **directory** — self-describing, portable, greppable, crash-surviving.
The dir is the ABI between every component, so any verb can be reimplemented alone.

```
items/<id>/
  manifest.json    # everything a worker + merger need (below)
  task.md          # the natural-language goal handed to the agent
  status           # pending|claimed|running|grown|blocked|verified|merged|failed|abandoned
  lease.json       # {host, pid, heartbeat_ts, deadline_ts}  (present while claimed)
  children         # (if grown) newline list of child ids
  result.json      # {verified, oracle_line, artifact, tokens, cost_cents, iters}
  log              # full agent stdout/stderr
  workdir/         # the isolated repo copy (or a pointer; see §7)
```

`manifest.json`:
```json
{
  "id": "weft.forms.valueasnumber-a",
  "parent": "weft.forms.valueasnumber",   // null for the seed
  "group": "weft.forms.valueasnumber",     // sibling variants → reduce keep-best
  "deps": [],                              // item ids that must be `verified` first
  "repo": "/path/to/weft",
  "edit_scope": ["src/script/forms-valueasnumber.lisp"],  // the ONLY files the agent may touch
  "oracle": "cd $WD && ... forms-oracle.lisp ... (run \"valueasnumber\")",
  "success": {"metric": "failed", "goal": 0, "direction": "min"},
  "model": "deepseek/deepseek-v4-flash",
  "budget": {"max_iters": 80, "max_cost_cents": 50, "timeout_s": 1500, "max_depth": 6},
  "merge": {"target": "src/script/forms-valueasnumber.lisp",
            "strategy": "keep-best-of-group | keep-if-improves"},
  "attempt": 1, "max_attempts": 2
}
```

Notes:
- **`edit_scope` is enforced, not trusted** (§3 merge membrane): the provisioner makes
  everything else read-only, and the merger rejects any item whose workdir diff touched
  a file outside `edit_scope` (defense in depth vs. a worker that wanders — cf. the
  worktree-symlink hazard).
- **A grown item** carries no `oracle`/`edit_scope` of its own; it's `done` when its
  `children` are `verified`. Whether the children actually *satisfy* the parent's
  intent (beyond each passing its own oracle) can be its own **review item** (§9).
- **`success`** is parsed from the oracle's tally line. Standardize the format so any
  oracle (CSS vectors, WPT subtests, test262 slices) emits `PASS=<n> FAIL=<n>`.

## 5. The pool (fungible workers)

Elastic `0..N`, stateless beyond the item dir. Each worker daemon:
```
loop:
  item = claim(next ready)           # atomic; §3, §6
  if none: sleep(jitter); continue
  provision(item)                    # reflink repo → $WD, wipe fasls, chmod scope
  heartbeat_start(item)              # background: touch lease every T seconds
  decision = agent(item)             # run operandi on task.md → CLOSE or GROW
  if GROW:  write children to items/, status=done-pending-children
  if CLOSE: {verified} = oracle(item) in isolated registry, wiped cache
            write result.json; status = verified | failed
  release(item)                      # roll up: parents whose children all verified → ready to close/merge
```
- **1 worker = a recursive agent**; **N workers = the same, parallel.** No swarm mode.
- **Kill -9 safe:** the lease expires → the item is re-claimable. A dead worker never
  wedges an item.
- Concurrency = number of worker processes; supervise with a `spawn N` + respawn loop.
  Mind the SBCL `--disable-debugger` gotcha (a background thread's unhandled condition
  kills the process — wrap the loop in `handler-case serious-condition`, let the
  supervisor respawn).
- **Start each worker/oracle from the dumped core** (`bin/operandi`, ~60 ms) not a cold
  `sbcl --load` — with the local-projects registry fix that's the difference between a
  ~16 s and a ~0.3 s floor per invocation, which dominates cost at 100× × iterations.
- Per-host count is bounded by **RAM** (each oracle load is a fresh SBCL, ~0.5–1 GB peak)
  and **cores**, not by the queue.

## 6. Coordination substrate

**A. Filesystem (default, zero-dep, local-first).** Claim = **atomic rename**
(`mv items/<id> running/<host>-<pid>/<id>`; the winner is whoever's rename succeeds,
everyone else gets ENOENT). State is directories: `items/ running/ verified/ merged/
failed/`. `ls` is the dashboard, `grep` the query engine; survives reboots and session
ends. Across hosts over a shared FS if rename is atomic there, else an
`O_CREAT|O_EXCL` lease file per item.

**B. Tiny TCP broker (when multi-host FS coordination gets painful).** A single-file
SBCL broker over `conch` (localhost + LAN) exposing `claim/heartbeat/complete`; the
item *dir* still holds the payload, the broker only arbitrates claims. Reuse
`natrium`/`conch` — no redis, consistent with local-over-containers.

Not Docker, k8s, Celery, or a cloud queue. The point is our boxes, our tools.

## 7. Disk at 100 workers (the real scaling wall)

100 × a repo copy is the actual constraint, not CPU or the queue (`weft` ≈ 105 MB →
~10 GB naive, plus fasl caches). Cheapest first:
1. **`cp --reflink=auto`** on a CoW fs (btrfs/xfs) — copies ~free until written; a
   worker only diverges by the one file it edits. **Preferred; confirm CoW on the
   target hosts first — it decides everything else here.**
2. **`git worktree`** per item — but heed the worktree-symlink hazard (a worker may
   symlink a gitignored data dir and a later merge clobbers the parent): forbid
   symlinks, absolute paths, `git show --stat` before any merge.
3. **overlayfs** (shared RO lower + per-worker upper) — fast, needs mount privileges.
4. Shared RO base + only `edit_scope` files copied writable, layered by the registry.

Cap fasl-cache disk (one wiped `XDG_CACHE_HOME` per worker); GC `verified/merged`
workdirs on a retention timer (keep `result.json` + `log`, drop `workdir/`).

## 8. The hard parts (where "handwavy" gets pinned)

- **Dependencies.** Splits aren't always independent. The grower declares `deps`; an
  item is claimable only when its deps are `verified` (deterministic readiness). This
  is the one place it's a tree *with edges*, not a flat list.
- **Termination / convergence.** The breathing must contract. Bound it: `max_depth`
  and `max_cost` caps (deterministic), plus an **escalation ladder** — a cheap model
  that fails to close a leaf N times re-items it on a stronger model; if that fails,
  `needs-human`. Runaway decomposition (a grower that keeps splitting) is caught by the
  depth cap.
- **Disjointness for merge.** Two leaves editing the same file conflict. The split
  should carve **disjoint injection points** (the weft self-registering-file pattern);
  where it can't, the parent adds a `dep` to serialize them. The model carves; the
  merge membrane rejects violations. Conflict-free by construction when carved right,
  caught deterministically when not.
- **Redundancy.** Two growers might produce near-duplicate sub-trees. Claiming prevents
  double-*owning* an item, but not overlapping *decomposition* — so either only one
  worker owns splitting a given item (claim the split), or a `dedup` review item folds
  siblings. Seeding a rough carve as a hint (§1) reduces this.
- **Non-determinism is a feature, not a bug.** Re-running a leaf yields a *different*
  diff — so `keep-best-of-N` (spawn N variant leaves in a `group`, reduce to the
  lowest-`failed`) instead of Spark-style deterministic recompute. The oracle is the
  acceptance test; retry is the fault model.

## 9. Verify vs. judge (models where there's no oracle)

Not every item has an objective oracle (a diff can pass tests and still be wrong in
intent, style, or side-effects tests don't cover). So a **review is just an item**,
and the grower picks a policy per surface:
- **oracle-only** — a `verify` item re-runs the oracle (deterministic; the default
  where a good oracle exists).
- **judge-only** — a `review` item is a model judgment (used where no oracle can
  decide; its verdict gates the parent).
- **oracle-then-judge** — the oracle gates *correctness*, a model judges the rest.

This is where "models over rules" and "determinism at the boundary" coexist without a
global choice: they're the same job machinery, composed per surface by the model that
grew the item.

## 10. Observability, cost, failure semantics

- **The list *is* the dashboard.** `status` counts by state; the tree's growth then
  shrinkage is the progress bar. Per-group best-so-far, live worker count, oracle-pass
  trend, **cumulative tokens + cost** (operandi already prints `[N iters, C¢, tok]` →
  capture into `result.json`). A `tail`-able event stream (one line per state
  transition) suits the Monitor pattern.
- **Budget ceiling** in the reaper: a per-day / per-plan spend cap; when hit, stop
  *claiming* (running items finish) and **report what was left un-grown — never
  silently truncate coverage.**
- **Failure semantics (must-haves):** worker dies → lease expires → item re-queued
  (bounded attempts), never wedged. Leaf un-closeable → escalation ladder (§8).
  Merge would regress canonical → revert, mark `needs-human`, keep the artifact.
  Everything **resumable**: relaunching any worker/reaper/merger picks up from the
  on-disk list; no in-memory-only progress.

## 11. Interfaces (target CLI)

```
operandi-swarm seed   --repo R --goal task.md [--carve hint.json]   # write items/<root>
operandi-swarm worker --host $(hostname) [--models flash,pro] [--max-ram 0.7]
operandi-swarm reap   [--lease-timeout 300 --budget-cents 20000]
operandi-swarm merge  --repo R [--commit]                            # the single-writer membrane
operandi-swarm status [--tree | --group weft.forms.* | --watch]
operandi-swarm spawn  --workers 100                                  # fork N supervised workers
```
`seed` writes the root item (optionally with a carve hint); everything else is grown.
Each verb is a small script/Lisp entrypoint; the item dir is the ABI between them.

## 12. Milestones

1. **The item ABI + a close/grow worker + claim + oracle, single-worker.** Port the
   forms wave: seed one root "make the forms surface pass", let one worker grow it into
   the six unit leaves and close them. Identical results to `forms-wave.sh`, now
   grown-not-authored. Regression guard.
2. **The pool + reap + a durable list** (§5–6, substrate A). Re-run the forms wave with
   N workers; identical results, now crash-safe and resumable.
3. **`spawn 100` + disk strategy** (§7 option 1). Prove 100 workers on one host against a
   large carve (a test262 or WPT subtree) without OOM/disk blowup.
4. **Grown decomposition + escalation ladder + keep-best** (§2, §8, §9). The seed→tree
   base case; auto-tiering; `keep-best-of-N`; review items where no oracle exists.
5. **`status --tree --watch` + cost accounting** (§10).
6. **(Optional) TCP broker** for multi-host (substrate B), reusing conch.

## 13. Open decisions for the implementer

- CoW filesystem availability on the target hosts (picks §7).
- Single-host-100 vs. multi-host-fan first (multi-host needs substrate B sooner).
- How aggressively to seed a carve hint vs. let workers grow from the monolith (start
  with a light hint to cut early redundancy; lean on pure growth as it proves out).
- Merge cadence: per-group-complete vs. streaming keep-if-improves.
- Where canonical commits land (direct-to-branch vs. a per-plan integration branch a
  human reviews) — default to an integration branch once volume is high.

---
*Prototype to read first:* `weft/tools/swarm/forms-{wave,worker,merge}.sh`,
`forms-wave2.sh` (variants + keep-best), and `weft/inspect/forms-oracle.lisp` (an oracle
emitting the tally line). The v1 primitives already work — this spec generalizes them
into a standing agenda. The host repo's `SWARM.md` carries the operating invariants
every item must still honor. The MCP `operandi_swarm` becomes a thin adapter over this native model.

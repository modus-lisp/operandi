# Agent Survey: Lessons from Hermes and Pi

*Survey date: 2026-08-02. A design reference for operandi (and skep), drawn from a
deep read of two notable open-source agents released around the same time:
NousResearch's **hermes-agent** (Python) and earendil-works's **pi**
(TypeScript). Read via six focused subagent passes (three per repo). This doc
captures the mechanisms worth stealing, the honest caveats, and a ranked roadmap.
Nothing here is committed to any of these projects yet — it's the plan.*

---

## Verdict

**pi is the reference for *how to build the harness*; hermes is the reference for
*what to add on top*; operandi should take pi's bones + hermes's brain, and keep
its own fitness-gate + ACP + dependency-free ethos as the differentiator neither
has.**

- **pi** — a coding-agent *toolkit* (our exact category), cleanly factored and
  honest about shipped-vs-designed. Its edge is architecture.
- **hermes** — a personal-assistant *product*. Its one real edge is an
  **autonomous self-improvement loop**; much of the rest is product surface.
- **operandi** — from-scratch CL coding agent, *architected for* self-improvement
  (held-out fitness harness + propose→fitness→keep|revert) but not yet doing it;
  has an ACP server, a tool-gate, cron, subagents, dependency-free crypto/stack.

## The three at a glance

| axis | hermes | pi | operandi |
|---|---|---|---|
| what it is | personal-assistant product | coding-agent harness/toolkit | from-scratch CL coding agent |
| real edge | autonomous self-improvement | clean architecture + honesty | fitness *gate* + dependency-free + ACP/skep |
| self-improvement | yes — background-review fork (ungated) | **no** — human-triggered extension | architected-for, not doing (has the gate) |
| memory | structured, autonomous writes | raw `AGENTS.md` context files, no curation | human-curated markdown + frontmatter |
| remote/protocol | one core, many surfaces (ACP/gateway/cron) | own **CBOR** session-multiplexer + own stdio-RPC (not ACP) | ACP + bare-TCP socket |
| permissions | approval system | **none by design** → containerize; trust-gate for config | tool-gate + ACP consent |
| durability | delivery + cron ledgers (shipped) | provisioned-ID append-log (**mostly design docs**) | transactional stores (pagetree/cabinet) + swarm spec |
| shipped vs design | mostly shipped | core shipped; fancy harness/hooks/durable are design docs | core shipped |

---

## Part 1 — Hermes (NousResearch/hermes-agent)

### 1.1 The closed learning loop (the headline — and the thing operandi lacks)

The entire "self-improving" story is **one mechanism: a post-turn background-review
*fork*.** After the user gets their answer, a second (cheap) LLM reads the finished
transcript and *proposes* skill/memory writes. Gated on a turn counter (~every 10),
never a quality signal.

- **Skills** = agentskills.io `SKILL.md` (YAML frontmatter + body), stored under
  `~/.hermes/skills/<category>/<name>/`. **Two-tier disclosure:** only
  `name + first ~57 chars of description` go into the system prompt; the body loads
  on demand via `skill_view`. Explicit create rule: "5+ calls, error overcome,
  user-corrected approach worked, non-trivial workflow." (`tools/skill_manager_tool.py`,
  `agent/background_review.py`, `agent/skill_utils.py`)
- **Skill self-improvement** = same fork; the feedback signal is the *transcript*
  ("user corrected my style/workflow," "a loaded skill was wrong — patch it NOW"),
  not a metric. Telemetry (`use_count`, `patch_count`) is used for *lifecycle*, and
  explicitly *not* as a quality signal ("use=0 is absence of evidence").
- **Memory** = flat `MEMORY.md` (agent notes) + `USER.md` (user facts),
  `§`-delimited, atomic writes; loaded once at session start into a **frozen
  snapshot** (prefix-cache stability) — no mid-session retrieval. Background fork
  writes autonomously (no human approval) with guards (injection scan, drift
  detection, capacity caps). (`tools/memory_tool.py`, `agent/turn_context.py`)
- **Anti-entropy curator** (`agent/curator.py`) = a *separate, slow* (~7-day) mostly
  deterministic pass: age-out stale skills, archive (never delete), optional LLM
  "umbrella-merge" of sibling skills — with a **dry-run → human-approve** gate.
- **Cross-session recall** = SQLite **FTS5 BM25 + bookends**, agent-initiated. Honest
  caveat: *no* LLM summarization/re-rank of results (thinner than marketing); the
  only summarization is in-lineage context compaction.
- **User modeling** = Honcho (external paid SaaS, per-turn LLM). Take the *taxonomy*
  (volatile summary / synthesized representation / durable conclusions), reject the
  dependency.

**The key relationship to operandi:** hermes's loop is high-recall / low-precision
(it edits constantly, cleaned up later by the curator + human dry-run). operandi's
held-out fitness harness is the *measured gate* hermes lacks. They are
**complementary**: transcript-review *proposes*, fitness *gates* the measurable
ones, human dry-run-approve handles the rest.

### 1.2 Execution engine

- **Loop** (`agent/conversation_loop.py`): standard ReAct; **stop = absence of
  tool_calls** (not `finish_reason`); a one-call "grace budget" past exhaustion so
  the model can wrap up; a separate `exit_reason` enum for observability.
- **Interrupt is three distinct features**: hard kill / redirect
  (rebuild-with-correction, discarding a stale in-flight response) / steer (soft
  inject). **Invariant: never re-inject interrupted chain-of-thought as replayable
  transcript `content`** — it reads as jailbreak-prefill and bricks the session
  (scaffolding rides an `api_content` sidecar).
- **Parallel tools via a path-overlap reservation planner**: split a batch into
  parallel vs sequential-barrier segments preserving emission order; file tools
  parallelize only if canonical paths don't overlap (writers reserve a path).
- **Programmatic Tool Calling (PTC) — the standout.** `execute_code` takes a Python
  string; a generated `hermes_tools.py` stub exposes each allowed tool as a function
  that RPCs over a Unix socket (auth token, tool allow-list, 50-call cap, env
  scrubbed) back into the *same* `handle_function_call`. **Only the child's stdout
  returns to the LLM** — 20 web pages / 50 reads never enter context; the whole
  script counts as one loop iteration. This generalizes our Workflow (a *fixed*
  pipeline) into an *agent-authored* one. (`tools/code_execution_tool.py`)
- **Subagents**: `delegate_task` = fresh in-process `AIAgent` on a thread (soft
  isolation: shared process/FS/creds, no hard kill); child summaries budget-capped
  vs parent headroom; async delegation re-injects as a fresh turn via a durable
  SQLite queue. A *separate* Kanban swarm = deterministic DB-driven task graph with
  detached OS-subprocess workers + a JSON-comment blackboard. **Same two-system
  split operandi reached (subagent vs Workflow) — validation.**
- **Trajectory/fitness**: `batch_runner`/`mini_swe_runner` "completion" is
  self-reported ("stopped calling tools"), **not graded** — a trap to avoid. The one
  real-graded bench (`mcp-research-data/`) attaches scenario pass/fail +
  `success_rescored` + efficiency costs (calls/tokens/$/s), head-to-head across
  configs. *That's* the pattern for operandi's fitness harness.

### 1.3 Connectivity & deployment

One synchronous `AIAgent` core wrapped by many thin async surfaces.
- **ACP** (`acp_adapter/`): both agent and client, `agent-client-protocol==0.9.0`,
  line-delimited JSON-RPC 2.0 over stdio. Full session lifecycle
  (`new|load|resume|fork|list|prompt|cancel|set_model|set_mode|set_config_option`).
  Does *not* serve `fs/*` or `terminal/*`. Caution: echoes its own protocol version
  rather than negotiating down. **→ skep can host hermes-acp today, unchanged.**
- **Gateway**: registry + ~5-method adapter ABC, no message bus, deterministic
  `build_session_key`, and a **delivery/obligation ledger** (durable at-least-once,
  `sha256(session|msg|content)` idempotency, pid+start-time liveness sweep, visible
  "♻️ recovered — may be a duplicate" marker).
- **Cron**: 60s in-process ticker; **advance-next-run before execute, under the
  store lock** (at-most-once, no crash-loop); **collapse missed runs into one
  catch-up**; `[SILENT]`/empty = no delivery; immutable execution ledger that marks
  interrupted runs `unknown` (never fabricates success).
- **Terminal backends**: tiny interface (`_run_bash → ProcessHandle` + `cleanup`),
  file ops derived from the shell; `local` = zero isolation, `docker` = the real
  boundary (cap-drop, no-new-privs, pids-limit, `--network=none`); "hibernate" =
  filesystem snapshot + provider-level RAM freeze (not portable).

---

## Part 2 — Pi (earendil-works/pi)

### 2.1 Architecture (the cleanest part)

Four sharply separated layers: **stateless building blocks** (`streamAssistant`,
tool phases) → **stateful composer** (`AgentHarness`/`AgentLane`, which inserts
durability writes *between* block phases) → **injected deps** (models, tools,
session, hooks — nothing hardcoded) → **transport, deliberately outside the core**.
The `AgentLane` is remotable by construction (every method async,
JSON-serializable, secret-free). Cleaner than "one loop with surfaces wrapped
around it" — but the executable internals (harness-v2, `driverLoop`, `resume`) are
**design docs, not shipped**; today's real code is `agent-loop.ts` + `agent-harness.ts`.

### 2.2 LLM layer — split API-dialect from provider-config

The single biggest leverage-per-line idea. An **API** = a wire dialect module
exporting `{stream, streamSimple}` (~10 exist). A **Provider** = runtime data:
`{id, baseUrl, auth, models: JSON catalog, api: adapter, compat}`. **DeepSeek and
OpenRouter are NOT separate adapters** — both are `openai-completions` + a baseUrl +
a `compat` flags block. The `compat` matrix (`packages/ai/src/types.ts`) turns
dozens of provider quirks into *declarative data* (`thinkingFormat` ×10,
`cacheControlFormat`, `supportsStrictMode`, `requiresToolResultName`, …).

- **Errors are in-stream terminal values, never thrown**: uniform `done|error` with
  `stopReason` + `errorMessage`.
- **Truncated-tool-JSON salvage cascade** (`JSON.parse → repairJson → partialParse →
  {}`) so a tool call can never break stream assembly; + **fail all tools on a
  `length` stop** (streamed args may validate while incomplete).
- **`transform-messages.ts`** = one normalization pass before every call
  (tool-call-id normalization, synthetic results for orphaned calls, dropping
  errored turns, same-model-only signature preservation).
- **Prompt caching**: a single moving *tail* cache breakpoint + stable system/tools
  breakpoints. Excellent *mechanism* — but it doesn't *enforce* the append-only
  prefix / stable toolset invariant that actually yields hits. **Pair it with
  hermes's "cache is sacred" discipline.**
- Weak parts: overflow + retry classification are **regex-over-error-strings**
  (brittle); token estimate is chars/4 (fine only because anchored on real `usage`);
  reasoning/signature handling is over-forked for 4 providers.

### 2.3 Session = append-only entry-*tree* (pi's strongest design)

A git-like DAG of immutable typed entries + a movable `leaf` pointer. Every state
change (model, thinking level, tool set) is a *new entry*, never a mutated field;
live state is **folded over the branch** (`deriveSessionContextState`) — impossible
to desync. Branching, "edit a past message and re-run," branch summaries, and
crash-safe resumption all fall out of one primitive. Resumption = per-turn
context-building (same code path as a fresh run). Backends: **JSONL**
(human-readable, git-diffable) + a rebuildable SQLite index.
(`packages/agent/src/harness/session/`)

### 2.4 Edit engine (steal wholesale)

`edit.ts`/`edit-diff.ts`: **uniqueness-as-anchor** (an `oldText` must match exactly
once or it errors and asks for context — no line numbers/hashes); **fuzzy fallback
that preserves unchanged bytes** (NFKC / smart-quote / CRLF normalization to still
match a retyped curly quote, but rewrites only touched lines); **reverse-order
multi-edit** (all matched against the original, applied back-to-front); **per-
canonical-path mutation queue** (concurrent sessions/worker-thread hazard). Toolset
is tiny: `read, bash, edit, write, grep, find, ls` (ripgrep-backed; no LSP/AST).

### 2.5 Compaction

Non-destructive, boundary-aware, usage-anchored: trigger left to the consumer;
cut-point walks back ~`keepRecentTokens`, **never severs a toolCall from its
toolResult**; a second compaction *iteratively updates* the prior summary; raw
entries stay in storage (navigate back to un-compact); summary runs with
`cacheRetention:none` + throwaway sessionId to avoid polluting the cache.

### 2.6 TUI — differential rendering

Line-granular (not cell): keep `previousLines[]`, scan for first/last changed line,
repaint only that window via **cursor-relative** moves (lives in normal scrollback,
not alt-screen), wrapped in **synchronized-output** (`\x1b[?2026h/l`) to kill
flicker; **full redraw is mandatory on width/height change** (wrapping invalidates
the diff). ~200 lines, no deps. Contrast: glass/VNC uses dirty-*tiles* for a pixel
framebuffer — granularity matched to the medium.

### 2.7 Remote protocol + durable harness + hooks

- **Remote protocol** (`packages/protocol|client|server`): 4-byte length prefix +
  one CBOR item; transport-neutral via a 2-method `ByteTransport` (only Unix-socket
  transport ships). **Snapshot-authoritative; `session_progress` is a transient
  hint; reconnect re-fetches** (never replay an event log to a late client).
  Session leases (exclusive/shared), multi-client per session. A *session
  multiplexer over a socket*, unlike ACP (methods over stdio, one agent/process).
  pi's coding-agent also has its own JSON-RPC-over-stdio mode → **an ACP shim makes
  pi hostable by skep**.
- **Durable harness** (`durable-harness.md`, **mostly design**): session-log replay,
  not a workflow engine. The sharp idea = **provisioned-ID / intent-before-effect**:
  write an intent record with pre-allocated deterministic IDs *before* an effect,
  append the result under those IDs *after*; recovery = `appendIfMissing` over the
  tail. Crash-idempotency without deterministic replay. Streams aren't resumable;
  non-idempotent tools must declare `replay:"safe"`. (Persistence substrate
  JSONL/SQLite ships; the recovery reducer is planned.)
- **Hooks** (`hooks.md`, mostly design): a typed event bus across the *whole* loop
  (`context`, `before_provider_payload`, `tool_call → block{reason}`, `tool_result`
  patch-chain, compaction/tree events), with a phantom-result-type so the event
  type declares whether a handler may mutate. Broader than operandi's tool-only hooks.

### 2.8 "Self-extensible" and permissions (honest)

"Self-extensible" = a rich **hot-reloadable typed extension API** (`registerTool`,
event interceptors, `registerCommand`, TUI widgets; permission gates, sandboxing,
and subagents are all *extensions*) + agentskills.io skills with two-tier disclosure
— but **human-triggered, no learning loop** (grepped clean for reflect/curator/
auto-skill). pi ships **no permission system by design** ("an in-process gate that
still shells to bash is theater; real isolation = OS/VM/container"), keeping only a
**project-trust load-gate** for config (extensions/skills/settings). Isolation is
punted to Gondolin micro-VM / Docker / OpenShell.

---

## Part 3 — Convergence & divergence

**Both independently converged on (→ validates operandi):**
- Two-tier skill disclosure (name/trigger in-context, body on demand).
- The two-system subagent split: LLM-delegated vs deterministic-orchestrated
  (hermes delegate/kanban · operandi subagent/Workflow · pi session-tree branches).
- Exact-string edits with a fuzzy fallback.
- Errors handled as data, minimal core toolset.

**They diverge on:**
- *Self-improvement*: hermes autonomous-but-ungated vs pi human-triggered. operandi
  has the gate to make the autonomous version safe.
- *Remote*: hermes reuses ACP; pi rolled its own CBOR multiplexer. operandi/skep sit
  on ACP — the interop lingua franca (skep can host both).
- *Permissions*: hermes approval / operandi tool-gate+consent / pi none-by-design.
  Different threat models; all "right" for theirs.

---

## Part 4 — Roadmap for operandi (ranked)

1. **Session as an append-only entry-tree, JSONL-backed** [pi]. Foundational — the
   substrate resume, branching, edit-rerun, *and* a self-improvement fork all ride
   on (the fork's critique becomes a `custom` branch entry). Highest structural win.
2. **Split `operandi.llm` into API-dialects + a declarative provider/compat table**
   [pi]. DeepSeek/OpenRouter/Anthropic with almost no per-provider code.
3. **Programmatic Tool Calling, Lisp-native in-image** [hermes]. We have a live
   REPL, so it's *simpler* than hermes's socket design: eval agent-authored Lisp that
   calls the tool functions directly, return only printed output. Keep the
   disciplines (allow-list, call-cap, timeout+kill, only-stdout).
4. **Background-review fork → the fitness gate** [hermes proposer + operandi gate].
   The self-improvement synthesis. Wire the transcript-reader to *propose* into
   fitness/approve, not to write blind. Fitness signal = external scenario pass/fail
   + efficiency costs + rescore path (the `mcp-research-data` pattern), never
   self-reported "done."
5. **Edit engine + non-destructive compaction + errors-as-in-stream-values** [pi].
   Robustness: uniqueness-anchor + fuzzy-preserve + reverse-order + per-path queue;
   boundary-aware compaction; `done|error` stream contract + truncated-JSON salvage
   + fail-tools-on-`length`.
6. **Crash-safety**: provisioned-ID / intent-before-effect idempotency for the
   [swarm](swarm-queue-spec.md) [pi] + a delivery/obligation ledger + cron
   advance-before-execute & collapse-missed [hermes].
7. **Widen hooks** to a `before_provider_payload` / `context` transform seam
   (phantom-result-type) [pi] — the missing place for redaction / injection-defense
   / cache-shaping. And keep `terminate` + `addedToolNames` on tool results,
   `before/after-tool` as the policy seam.
8. **Interrupt = three features** (kill / rebuild-with-correction / soft-steer) and
   never replay interrupted CoT as content [hermes] — if/when operandi supports
   mid-run operator nudges.

## Part 5 — Roadmap for skep

- Adopt pi's **snapshot-authoritative / progress-is-a-hint / reconnect-refetches**
  contract for host↔watcher fan-out (ACP-over-stdio doesn't give multi-client this).
- **Host pi *and* hermes as ACP agents** — both have stdio JSON-RPC modes; low-effort
  interop wins and two more conformance tests (after operandi + goose).
- Steal hermes's **delivery ledger** for the lossy relay (published-but-unacked event
  = the same failure class).
- Keep the "isolate at the OS, prompt-injection is expected" posture for untrusted
  Nostr inbound [pi].

## Part 6 — Free invariants to adopt now

- **"Prompt-prefix caching is sacred"** [hermes]: never mutate past context, swap
  toolsets, or rebuild the system prompt mid-run except for compression. + the
  frozen-memory-snapshot-at-session-start trick.
- **Narrow-waist core** [hermes]: every core tool ships on every call; new capability
  = skill/plugin/CLI, not core surface.
- **Project-trust config load-gate** [pi]: gate loading project-local
  memory/subagents/extensions on a cached trust decision so a cloned repo can't
  silently reconfigure the agent. Reframe operandi's tool-gate as guardrail (for
  interactive/in-editor consent) not a security boundary; point untrusted/unattended
  use at containers.

---

## Appendix — what to skip, and honesty flags

**Skip:** Honcho SaaS (take the taxonomy, not the dependency); hermes's
`SubagentLifecycleService` HMAC "contract" façade; tiered tool disclosure (until
dozens of MCP tools); `toolset_distributions` (a data-gen sampler, not routing);
provider-coupled serverless-snapshot glue; gateway identity kludges (a Nostr pubkey
already gives skep stable cross-surface identity); pi's over-forked
reasoning/signature handling (unify at our provider count).

**Design-not-shipped in pi:** the durable-harness recovery reducer, the generic
whole-loop hooks system, harness-v2 internals, and the stateless-blocks/composer
refactor are design docs — borrow the *shapes*, not the unbuilt detail (the
JSONL/SQLite persistence substrate *is* shipped).

**Thinner-than-marketing in hermes:** recall is BM25+bookends (no LLM summarization);
self-improvement has no measured gate (turn-counter + 7-day janitor + human
dry-run — exactly where operandi's fitness harness is genuinely ahead); subagent
isolation is soft (shared process/FS/creds, no hard kill); "runs anywhere" ≠
"isolated anywhere."

**Where operandi is already ahead — don't regress:** the held-out fitness harness
(a measured self-improvement gate neither has), transactional durable stores
(pagetree/cabinet sidestep pi's JSON-file scar tissue and much of the durable-log
machinery), worktree-isolated subagents (closer to real isolation than in-process
threads), and a dependency-free / self-hosted posture.

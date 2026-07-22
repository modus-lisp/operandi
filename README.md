# operandi

A Lisp-native ReAct agent loop — "Claude Code in Common Lisp."

A small local LLM (llama.cpp) or a frontier model (via OpenRouter) drives
a tool-calling loop against the OpenAI-compatible `/v1/chat/completions`
protocol. The agent reads/writes/edits files, runs shell commands,
searches code and the web — and, the sharp edge, **evaluates arbitrary
Common Lisp in the running SBCL image**, so it can call straight into
whatever domain packages the host application has loaded.

## Layout

```
operandi.asd          ASDF system :operandi
src/
  llm.lisp            operandi.llm      — backend-agnostic chat client (llama / OpenRouter)
  store.lisp          operandi.store    — SQLite audit log (agent_tool_calls)
  search.lisp         operandi.search   — Brave Search (backs WebSearch)
  hooks.lisp          operandi.hooks    — pre/post-tool hooks (default: log to SQLite)
  tools.lisp          operandi.tools    — tool registry: Read/Write/Edit/Bash/Grep/Glob/
                                          WebFetch/WebSearch/Remember/TodoWrite/Eval
  engine.lisp         operandi.engine   — the agent loop (retries, auto-compaction, cost)
  subagent.lisp       operandi.subagent — the Task tool (delegated sub-agents, depth-capped)
  cron.lisp           operandi.cron     — in-image scheduled-task runner (empty by default)
  session.lisp        operandi.session  — conversation session state + persistence (shared)
  tui.lisp            operandi.tui      — interactive REPL: streaming, tool rendering, /commands
  acp.lisp            operandi.acp      — Agent Client Protocol server (drives operandi from editors)
bin/operandi.lisp     standalone CLI          (fast launcher: bin/operandi)
bin/operandi-acp.lisp ACP server entry point  (fast launcher: bin/operandi-acp)
mcp/                  Node MCP server exposing operandi as dispatchable tools + a swarm harness
```

## Use as a library

```lisp
(ql:quickload :operandi)
(operandi.store:open-store)                 ; opens ~/.operandi/operandi.db
(operandi.engine:run "Summarize what's in the current directory.")
```

By default the backend is a local llama.cpp on `http://127.0.0.1:8081`.
Switch to OpenRouter (token at `~/.operandi/openrouter.token`):

```lisp
(operandi.llm:use-openrouter :model "minimax/minimax-m2.7")
(operandi.engine:run "...")
```

The **Eval** tool sees every package loaded in the image. To give the
agent domain reach, load your own systems before calling `run` — then it
can `(my.domain:some-fn ...)` directly from a tool call. To flavor its
instructions, rebind `operandi.engine:*base-system-prompt*`.

## CLI

```
sbcl --non-interactive --load bin/operandi.lisp -- "your task here"
sbcl --non-interactive --load bin/operandi.lisp -- tui
sbcl --non-interactive --load bin/operandi.lisp -- --openrouter minimax/minimax-m2.7 "task"
```

### Fast start (`bin/operandi`)

Loading from source every run is slow — not SBCL (≈50ms) but the per-run
Quicklisp dependency load (dexador pulls in cl+ssl/cffi/usocket), plus, if your
`~/.config/common-lisp` points an ASDF `:tree` at a big directory, a full
filesystem scan on every launch. `bin/operandi` sidesteps both: it dumps a
**core image** with operandi (+ linedit) baked in and launches from it with the
init files skipped — **≈60ms to a ready agent** instead of ~16s.

```
bin/operandi tui
bin/operandi "your task here"
bin/operandi --openrouter deepseek/deepseek-v4-flash "task"
```

The core (`operandi.core`, gitignored) is built on first run and rebuilt
automatically whenever a source file changes. Same arguments as the `.lisp`
entry point above.

## TUI

`tui` (aliases: `shell`, `repl`) launches the interactive REPL — an enhanced,
inline terminal UI:

- **type while it works** — on an interactive tty the input line is pinned to
  the bottom row (a scroll region keeps it separate from streaming output) and
  the agent turn runs on a background thread, so you can compose the next message
  mid-turn. What you type is echoed as `⧗ queued` and submitted as the next user
  turn when the current one (with all its tool use) finishes. `^C` aborts the
  running turn (and drops the queue); `^D` quits. Piped/non-tty input falls back
  to a simple line-at-a-time loop.
- **live token streaming** of the assistant's reply,
- **coloured tool-call lines** — `⏺ Bash(echo hi)` … `✔ 27ms  hi` — rendered as
  each tool fires (via `operandi.hooks`, so nothing in the engine changed),
- **multi-turn memory** — conversation history is threaded across turns until
  `/clear`,
- a per-turn metrics line and a **running session cost** in the prompt,
- **Ctrl-C aborts the current turn** and returns to the prompt (Ctrl-D quits),
- slash commands: `/help /clear /sessions /resume [id] /cost /model [id] /system /tools /quit`,
- each session is saved to `~/.operandi/sessions/<id>.{md,json}`, updated after
  every turn (so a crash never loses it); `/clear` starts a new one. `/sessions`
  lists them and `/resume [id]` continues one (the latest if no id) — or resume
  from the shell: `bin/operandi --resume [id] tui`, or non-interactively
  `bin/operandi --resume [id] "a follow-up question"`.

It's line-based on purpose — no alt-screen, no raw mode — so it works over ssh
and in a pipe. Colour and line-editing (linedit, if installed) auto-enable only
on an interactive tty; piped/non-interactive output is plain text.

```
sbcl --non-interactive --load bin/operandi.lisp -- --openrouter deepseek/deepseek-v4-flash tui
```

## ACP server (drive operandi from an editor)

operandi speaks the [Agent Client Protocol](https://agentclientprotocol.com) —
the "LSP for coding agents" — so editors like Zed can drive it in their agent
panel. It's JSON-RPC 2.0 over stdio: the editor spawns `bin/operandi-acp` and
exchanges `initialize` / `session/new` / `session/prompt` / `session/cancel`,
while operandi streams `session/update` notifications back (message chunks, tool
calls, the plan, usage) and asks the editor for permission before edits/commands
via `session/request_permission`. Sessions persist and resume (`session/load`).

```jsonc
// example Zed agent-server entry
{ "operandi": { "command": "/path/to/operandi/bin/operandi-acp" } }
```

`bin/operandi-acp` launches from a dumped core (~60 ms). Backend: set
`OPERANDI_ACP_MODEL` (an OpenRouter model id), else it uses deepseek-v4-flash if
an OpenRouter token is present, else local llama.cpp. The mapping — operandi's
streaming/hooks/plan/usage/session seams onto the ACP wire — lives in
`src/acp.lisp`; `(operandi.acp:serve)` runs it over any stdin/stdout.

## MCP server

`mcp/server.js` spawns the CLI per task and exposes `operandi_run` /
`operandi_fan` / `operandi_swarm` tools over stdio. Point it at a
different loader with `OPERANDI_ROOT` (default: the repo root above `mcp/`).

```
cd mcp && npm install && node server.js
```

## Tests

`inspect/robustness-test.lisp` is the tool-sandbox oracle — it asserts that
a runaway Eval (infinite loop / stack overflow) or a hanging Bash command
comes back as an error string instead of freezing or crashing the agent.
Exits 0/1, so it doubles as a swarm oracle:

```
sbcl --non-interactive --load inspect/robustness-test.lisp
```

## Configuration

| What | Where |
|------|-------|
| OpenRouter token | `~/.operandi/openrouter.token` |
| Brave Search token | `~/.operandi/brave-search.token` |
| Agent audit log DB | `~/.operandi/operandi.db` |
| Persistent agent notes | `~/.operandi/operandi-notes.md` |
| TUI session transcripts | `~/.operandi/sessions/` |
| Compaction offload | `~/.operandi/offload/` |
| Cron reports / state | `~/.operandi/` |

Tool-sandbox limits (rebindable specials in `operandi.tools`):
`*bash-timeout*` (120s — a Bash command is killed past this),
`*eval-timeout*` (60s — an Eval form is aborted past this).

## License

MIT

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
bin/operandi.lisp     standalone CLI
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
sbcl --non-interactive --load bin/operandi.lisp -- shell
sbcl --non-interactive --load bin/operandi.lisp -- --openrouter minimax/minimax-m2.7 "task"
```

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
| Cron reports / state | `~/.operandi/` |

Tool-sandbox limits (rebindable specials in `operandi.tools`):
`*bash-timeout*` (120s — a Bash command is killed past this),
`*eval-timeout*` (60s — an Eval form is aborted past this).

## License

MIT

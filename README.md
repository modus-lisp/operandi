# operandi

A Lisp-native ReAct agent loop — "Claude Code in Common Lisp."

A small local LLM (llama.cpp) or a frontier model (via OpenRouter) drives
a tool-calling loop against the OpenAI-compatible `/v1/chat/completions`
protocol. The agent reads/writes/edits files, runs shell commands,
searches code and the web — and, the sharp edge, **evaluates arbitrary
Common Lisp in the running SBCL image**, so it can call straight into
whatever domain packages the host application has loaded.

## Quick start

`bin/operandi` is the entry point. It dumps a **core image** with operandi
baked in and launches from it with the init files skipped, so a run costs
~20 ms of launcher overhead instead of ~16 s of Quicklisp loading (the model
preflight adds a network round-trip on top). The core (`operandi.core`,
gitignored) is built on first run and rebuilt automatically whenever a source
file changes.

```
bin/operandi                                      # interactive TUI (the default)
bin/operandi "summarize what's in this directory" # one-shot task
bin/operandi -p "summarize this directory"        # same, prompt spelled out
bin/operandi --openrouter deepseek/deepseek-v4-flash "task"
bin/operandi --openrouter deepseek/deepseek-v4-flash --effort low --repl
```

With no prompt at all, `bin/operandi` opens the TUI. The default backend is
a local llama.cpp on `http://localhost:9931`; `--openrouter` switches to
OpenRouter (token at `~/.operandi/openrouter.token`).

`--llama ADDR` names that backend explicitly and points it at another server.
ADDR is the server's address, not a model — a llama.cpp server usually has
exactly one model loaded, so there is nothing to name:

```
bin/operandi --llama :9931 "task"                   # localhost:9931
bin/operandi --llama localhost:9931 "task"          # same thing, spelled out
bin/operandi --llama http://box:9931 "task"         # another host
bin/operandi --llama http://box:9931/v1/chat/completions "task"
bin/operandi --llama :9931 -m qwen3-32b "task"      # multi-model server
```

It is `[proto://][host][:port][/path]`; a missing scheme means `http`, a
missing host means `localhost`, and a missing path means `/v1/chat/completions`.
At least one component is required — a bare `--llama` is an error, not a
request for the default. **A port must be colon-prefixed**: `:9931`, not
`9931`. A bare number is rejected rather than guessed at, because a forgotten
colon is far more likely than a hostname that is all digits. For the rare
multi-model server (llama.cpp's router mode, or a proxy in front of several),
`-m MODEL` names the model to send.

Without `-m`, operandi asks the server: it reads `GET <base>/v1/models` and
picks from what comes back — the only model if there is one, otherwise the
largest, preferring one the server already has loaded (naming an unloaded
model makes the router load it, a multi-gigabyte side effect nobody asked
for). A server that does not implement `/v1/models` is not an error: the
request then carries no model field at all, which is exactly right for a
single-model llama.cpp server.

### Running a specific model on a specific port

operandi does not start the server — it is a client. Start llama.cpp yourself
with the model and port you want, then point operandi at it:

```
llama-server -m /path/to/model.gguf --host 127.0.0.1 --port 9931 \
             --alias my-model --jinja -c 65536 -ngl 99
bin/operandi --llama :9931 "task"         # or: --llama :9931 -m my-model
```

`--alias` is the name the server reports in `/v1/models` and accepts as the
`model` field, so it is what `-m` should say. `-c` is the context size and
`-ngl` the number of layers to offload to the GPU (`99` = all of them). The
default port is 8080; this repo's default is 9931, which is why `--llama :9931`
is the usual spelling. Any localhost port works — the sandbox allows loopback
outbound generally, so a server on 8080 or 12345 is reachable without touching
`bin/sandbox.sb`. `--host 0.0.0.0` exposes the server to the LAN, which the
sandbox does *not* reach.

llama.cpp's **router** mode serves several models from one port, loading them
on demand, and is what the `--llama :9931` default here talks to:

```
llama-server --models-preset ~/models.ini --host 127.0.0.1 --port 9931
```

With a router, `--llama :9931` with no `-m` asks the server which model to use
(see above); `-m <id>` names one outright. Naming an unloaded model makes the
router load it, which takes as long as loading a multi-gigabyte file does.

### From source (no core)

The same arguments work through the `.lisp` entry point, at the cost of a full
Quicklisp load each run — useful when you want to run against a modified tree
without waiting for a core rebuild, or when the core can't be written:

```
sbcl --non-interactive --load bin/operandi.lisp -- "your task here"
sbcl --non-interactive --load bin/operandi.lisp -- --repl
```

`bin/operandi --help` prints the same list of flags, commands and environment
variables as the tables below, from the running binary. It is sectioned and
coloured on a tty (plain when piped, or with `NO_COLOR` set), and
`--help TOPIC` prints one section in full: `modes`, `backends`, `session`,
`agent`, `env`, `files`, `diagnostics`, or `all`.

## CLI

```
operandi [FLAGS] ["task description"]
```

There are no bare command words. Every mode is a flag, so a bare word is
always prompt text — `operandi tui` is a task whose text is "tui".

| Mode | Meaning |
|------|---------|
| *(no prompt)* | interactive TUI — the default |
| `"task description"` | a one-shot task; the remaining arguments are joined into the prompt |
| `--prompt TEXT`, `-p TEXT` | the same, spelled out; giving both is an error |
| `--ask "question"` | the question phase, non-interactively: rival hypotheses → a swarm settles them → an answer from the verdicts |
| `--repl` | the interactive TUI, explicitly; a prompt together with `--repl` is an error |
| `--ask-json` | the same, as data: a JSON request on stdin (`question`, optional `hypotheses`, `context`, `synthesize`, `worker_model`), the result as one `@@OPERANDI-JSON@@ {...}` line |
| `--mcp-status` | list the external MCP servers: where each came from, whether it started, its tools |
| `--topup ppq AMOUNT [cents\|sats]` | buy PPQ credit over Lightning, then exit (`[--to NIP05] [--no-wait]`) |

**Flags** (any order):

| Flag | Meaning |
|------|---------|
| `--llama ADDR` | use the local llama.cpp server at `ADDR` = `[proto://][host][:port][/path]` (e.g. `:9931`, `http://box:9931`); at least one component required, and a port must be colon-prefixed |
| `-m MODEL` | model id to send; only for a multi-model llama.cpp server (without it, the model is asked of the server — see above) |
| `--openrouter [MODEL]` | use OpenRouter; `MODEL` is a `vendor/name` slug. Bare `--openrouter` uses the configured model (see below) |
| `--routstr [NODE-URL] [MODEL]` | use Routstr (Lightning-paid nodes); `NODE-URL` picks the node |
| `--ppq [MODEL]` | use PPQ (ppq.ai), OpenAI-compatible, paid in Bitcoin/Lightning |
| `--tinfoil [MODEL]` | use Tinfoil's own enclave, sealed end to end (no plaintext mode) |
| `--maple [ADDR] [MODEL]` | use Maple through a **local** `maple-proxy` at `ADDR` (default `127.0.0.1:8080`). The proxy does Maple's attested transport; operandi does **not** verify the enclave, so `--confidential` refuses it. Plaintext to a non-loopback host is refused. Optional key in `~/.operandi/maple.token` |
| `--confidential` | seal every request to a key bound into a hardware attestation that operandi verifies, and refuse any path that can't be sealed that way. The sealed endpoint's operator must also be in `--trust`. Today that's Tinfoil's router: `--tinfoil`, `--ppq private/MODEL` or `--routstr tinfoil-MODEL` |
| `--trust=LIST` | the operators trusted with the cleartext: `maple-routing` (needs both Tinfoil names: it routes some traffic to Tinfoil's router), `tinfoil-untrusted` (Tinfoil's shared router, whose safeguards sidecar reads transcripts and whose config and reports cross outside any enclave; needs `tinfoil-inference`, the model enclaves it forwards to), `tinfoil-inference` (alone it enables nothing; nothing reaches the model enclaves directly). A name without what it needs is an error |
| `--rc TARGET` | mirror each turn to a Marmot chat (npub/nprofile/NIP-05 starts one; a chat name or group id joins one) |
| `--worker-model M` | model for sub-agents: a `vendor/name` slug, `llama`, or `inherit` |
| `--effort off\|low\|medium\|high\|default` | reasoning effort (also `OPERANDI_EFFORT`) |
| `--resume [ID]` | resume a saved session (latest if no ID); with a task, runs one more non-interactive turn |
| `--tools A,B,C` | allow-list: exactly these tools (`mcp__rea__*` takes every tool with that prefix) |
| `--no-tools X,Y` | the default tool set minus these |
| `--repl` | the interactive TUI, explicitly (`--tui` is an older name for it) |
| `--help`, `-h` `[TOPIC]` | usage; `TOPIC` is one of `modes`, `backends`, `session`, `agent`, `env`, `files`, `diagnostics`, `all` |
| `--model M` | model `M` on the configured backend (`config.json` `backend`: `ppq` or `openrouter`), so a harness need not know which this machine pays |
| `--mcp-config FILE` | also read FILE's `mcpServers` (repeatable; after the default file) |
| `--mcp NAME=CMD [ARGS…]` | an external stdio MCP server, quoted as one argument (repeatable) |
| `--mcp NAME=https://URL` | an external Streamable HTTP MCP server |
| `--no-default-mcp` | do not read `~/.operandi/mcp.json` |

An unknown `--flag` is rejected (exit 2) rather than sent to the agent as task
text. Before any path that runs a turn, the model is preflighted — a bad slug
or an unreachable local server fails loud here instead of blank-per-turn
(`OPERANDI_NO_PREFLIGHT=1` skips the check).

`--effort` sets the model's reasoning: on OpenRouter `off` sends
`reasoning.enabled=false` and the levels send `reasoning.effort`; on llama.cpp
anything but `off`/`default` turns Qwen-style thinking on. The
context-compaction budget follows the backend (24k tokens on llama, 96k on
OpenRouter) unless `OPERANDI_CONTEXT_BUDGET` is set.

Environment variables are read when the image loads. On the fast path that
means the core build, so `OPERANDI_EFFORT` / `OPERANDI_CONTEXT_BUDGET` set at
run time are ignored unless the core was built with them — prefer the flags or
the `/effort` command.

| Variable | Meaning |
|----------|---------|
| `OPERANDI_MAX_TOKENS` | per-turn output cap (default 16384) |
| `OPERANDI_CONTEXT_BUDGET` | compaction threshold in tokens |
| `OPERANDI_CONTEXT_HEADROOM` | fraction of the budget kept free (default 0.2) |
| `OPERANDI_MAX_ITERS` | hard cap on the agentic loop (default 500) |
| `OPERANDI_WRAPUP_ITERS` | model turns granted after that cap to write findings up before the run stops (default 3; 0 = stop at the cap) |
| `OPERANDI_EFFORT` | same as `--effort` |
| `OPERANDI_WORKER_MODEL` | same as `--worker-model` |
| `OPERANDI_SEARCH` | web-search backend: `openrouter`, `searxng`, `brave`, `auto` |
| `OPERANDI_SWARM_DEADLINE` | seconds a Fan/Investigate batch may run (default 900) |
| `OPERANDI_NO_PREFLIGHT=1` | skip the model preflight |
| `OPERANDI_FANCY_TUI=1` | opt in to the scroll-region TUI |
| `OPERANDI_SBCL` | the `sbcl` binary the launcher uses |

`bin/operandi-acp` and `bin/operandi-nostr.lisp` have their own variables
(`OPERANDI_ACP_MODEL`, `OPERANDI_NOSTR_*`); see their sections below.

### Configuration

`~/.operandi/config.json` sets the default model:

```json
{ "model": "deepseek/deepseek-v4.1-flash" }
```

It is applied unless the command line names a backend. `"llama"` selects the
local backend; a `vendor/name` slug selects OpenRouter. A bare `--openrouter`
uses the configured model if it is a slug, else `minimax/minimax-m2.7`.

## Use as a library

```lisp
(ql:quickload :operandi)
(operandi.store:open-store)                 ; opens ~/.operandi/operandi.db
(operandi.engine:run "Summarize what's in the current directory.")
```

`operandi.engine:run` takes the prompt plus `&key system tool-names
max-iterations verbose history` and returns `(values final-text
full-message-history n-iterations)`.

Switch to OpenRouter (token at `~/.operandi/openrouter.token`):

```lisp
(operandi.llm:set-backend-openrouter! :model "minimax/minimax-m2.7")
(operandi.engine:run "...")
```

The **Eval** tool sees every package loaded in the image. To give the
agent domain reach, load your own systems before calling `run` — then it
can `(my.domain:some-fn ...)` directly from a tool call. To flavor its
instructions, rebind `operandi.engine:*base-system-prompt*`.

## Layout

```
operandi.asd          ASDF system :operandi (and :operandi/nostr)
src/
  llm.lisp            operandi.llm      — backend-agnostic chat client (llama / OpenRouter)
  store.lisp          operandi.store    — SQLite audit log (agent_tool_calls)
  search.lisp         operandi.search   — web search (OpenRouter plugin / SearXNG / Brave)
  hooks.lisp          operandi.hooks    — pre/post-tool hooks (default: log to SQLite)
  tools.lisp          operandi.tools    — tool registry + the tool-sandbox limits
  mcp.lisp            operandi.mcp      — MCP client: external servers' tools as operandi tools
  engine.lisp         operandi.engine   — the agent loop (retries, auto-compaction, cost)
  subagent.lisp       operandi.subagent — Task/Fan/Spawn/Investigate/SendMessage (depth-capped)
  cron.lisp           operandi.cron     — in-image scheduled-task runner (empty by default)
  session.lisp        operandi.session  — conversation session state + persistence (shared)
  tui.lisp            operandi.tui      — interactive REPL: streaming, tool rendering, /commands
  acp.lisp            operandi.acp      — Agent Client Protocol server (drives operandi from editors)
  actor.lisp http.lisp safefetch.lisp sessiontree.lisp text.lisp   — supporting modules
bin/operandi.lisp     standalone CLI          (fast launcher: bin/operandi)
bin/operandi-acp.lisp ACP server entry point  (fast launcher: bin/operandi-acp)
mcp/                  Node MCP server exposing operandi as dispatchable tools + a swarm harness
                      (mcp/test/fixture-server.mjs: a test server for the MCP client)
```

The default tool set is `Eval Read Write Edit Bash Grep Glob WebFetch
WebSearch Remember Findings Task Fan Spawn Investigate SendMessage TodoWrite`
(`Verdict` is deliberately absent — it is the worker's reporting channel, added
by `Investigate` to the workers it spawns).

## macOS sandbox

The agent runs its tools (Bash, Eval, file writes) with whatever rights your
user has, so on macOS you can run it inside `sandbox-exec` instead.
`bin/operandi-sandbox-macos` is the launcher; `bin/sandbox.sb` is the profile.

Put it on your PATH:

```
ln -s ~/workspace/modus-lisp/operandi/bin/operandi-sandbox-macos ~/.local/bin/operandi
```

Then run `operandi` as usual, from the project you want it to work on:

```
cd ~/projects/myapp && operandi
```

What the sandbox allows:

- **Writes** only to the directory you launched from (the project), this
  repo, `~/.operandi` (sessions, token, database), `~/quicklisp`, the fasl
  cache `~/.cache/common-lisp`, `/tmp`, and the per-user temp/cache dirs.
- **Reads** of the OS, `/usr`, `/opt`, your git config, and the rest of the
  home directory's metadata. Other files in `~` are not readable.
- **Network**: any public host, plus any port on `localhost` — so a llama.cpp
  server on 9931, SearXNG on 8080, or a dev server on any other loopback port
  is reachable. The agent may also bind a local dev server on localhost.
- **No AppleScript** to other apps. The clipboard is readable for Ctrl-V.

Things to know:

- The launcher refuses to expose `$HOME` or `/`. If you launch from there, it
  falls back to running inside the repo.
- The profile is a template. The launcher fills in the home, repo, temp and
  cache paths on every run, so nothing is hardcoded to one user.
- The core is rebuilt **before** the sandbox starts, since the sandbox can't
  see `~/quicklisp` or write the fasl cache.
- A profile change takes effect only on the next launch. A session already
  running keeps the profile it started with.
- Nested `sandbox-exec` is denied, so you can't test the profile from inside
  an already-sandboxed session. Test it from a normal shell.
- Paths are matched after symlink resolution (`/etc` is `/private/etc`), so
  rules written against the symlink names won't match.

## TUI

`tui` (aliases: `shell`, `repl`) launches the interactive REPL — an enhanced,
inline terminal UI:

- **live token streaming** of the assistant's reply,
- **coloured tool-call lines** — `⏺ Bash(echo hi)` … `✔ 27ms  hi` — rendered as
  each tool fires (via `operandi.hooks`, so nothing in the engine changed),
- **multi-turn memory** — conversation history is threaded across turns until
  `/clear`,
- a per-turn metrics line and a **running session cost** in the prompt,
- **Ctrl-C aborts the current turn** and returns to the prompt (Ctrl-D quits),
- **Ctrl-C keeps the partial turn** — the tool calls that ran stay in context,
  marked as interrupted, so the next prompt knows what already happened,
- **multi-line input**: Ctrl-J inserts a newline, Enter submits; Up/Down move
  between lines of the buffer and reach history from the first/last line.
  A pasted block arrives as one message,
- **images**: `@path/to/shot.png` in a prompt attaches it (drag a file onto
  the terminal to paste its path); **Ctrl-V** pulls an image off the clipboard
  (`brew install pngpaste`), as does `/paste`. Needs a vision model —
  `deepseek/deepseek-v4.1-flash` and the Qwen-VL family on OpenRouter are,
- **web search** follows the model: on OpenRouter the `WebSearch` tool uses
  OpenRouter's web plugin (same credential, ~$4/1000 results); on local llama
  it uses a SearXNG at `SEARXNG_URL` (default `http://127.0.0.1:8080`) if one
  answers, else Brave. `OPERANDI_SEARCH=openrouter|searxng|brave` or `/search`
  forces one,
- slash commands: `/help /clear /sessions [all] /resume [id] /cost /model [id] /effort [lvl] /workers [m] /paste /search [be] /system /tools /ask <question> /quit` (`/exit` and `/q` also quit),
- each session is saved to `~/.operandi/sessions/<id>.{md,json}` from the
  first real message on and checkpointed after every tool result during a
  turn, so a crash or kill mid-turn loses at most a couple of seconds;
  `--resume` closes a cut-off turn cleanly. `/clear` starts a new one. `/sessions`
  lists them and `/resume [id]` continues one (the latest if no id) — or resume
  from the shell: `bin/operandi --resume [id] tui`, or non-interactively
  `bin/operandi --resume [id] "a follow-up question"`.

It's line-based on purpose — no alt-screen — so it works over ssh and in a pipe.
Colour and line editing ([quill](https://github.com/modus-lisp/quill): Emacs keys,
Ctrl-J for a newline, history with Ctrl-R, Ctrl-V to attach a clipboard image)
switch on only on an interactive tty; piped/non-interactive input and output are plain.

The default interactive UI is the robust line REPL (plain writes, no cursor
positioning). A concurrent UI — input line pinned to the bottom row, the turn
on a background thread, so you can **type while it works** — is opt-in via
`OPERANDI_FANCY_TUI=1`; it corrupts output under some terminal/multiplexer
combos (e.g. tmux scroll-region interactions), which is why it isn't the default.

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

`bin/operandi-acp` launches from a dumped core (~30 ms). Backend: set
`OPERANDI_ACP_MODEL` (an OpenRouter model id), else it uses deepseek-v4.1-flash if
an OpenRouter token is present, else local llama.cpp. The mapping — operandi's
streaming/hooks/plan/usage/session seams onto the ACP wire — lives in
`src/acp.lisp`; `(operandi.acp:serve)` runs it over any stdin/stdout.

Methods: `initialize`, `authenticate`, `session/new`, `session/load`,
`session/prompt`, and the `session/cancel` notification. Unknown methods get
JSON-RPC `-32601`; an unknown session id on `session/prompt` gets `-32602`
(`session/load` reports it as `-32603`). `session/new` accepts a
non-standard `systemPrompt`, which hosts such as Buzz use to configure the agent.

Tests (no model needed unless noted):

```
sbcl --core operandi.core --noinform --no-userinit --no-sysinit --non-interactive \
  --load inspect/acp-test.lisp        # handler oracle: spec shapes, streamed updates, permission gate
sbcl --core operandi.core --noinform --no-userinit --no-sysinit --non-interactive \
  --load inspect/acp-wire-test.lisp   # wire level: JSON-RPC in/out over string streams, error codes
cd inspect/acp && npm install && npm test   # schema conformance (needs a model backend)
```

`inspect/acp-test.lisp` and `inspect/acp-wire-test.lisp` run in `inspect/fitness.sh`.
The schema-conformance suite and the goose interop script are not part of fitness.
See `inspect/acp/README.md` for the three validation layers.

## External MCP servers (operandi as an MCP client)

operandi can use the tools of any MCP server. Each one the server lists becomes
an ordinary tool named `mcp__<server>__<tool>` in the agent's default set; a call
is a `tools/call` on the server, and the result comes back labelled as external
content (data, never instructions).

**Where servers come from**, later winning per server name:

1. the default file, `~/.operandi/mcp.json` (or `$OPERANDI_MCP_CONFIG`); skip it
   with `--no-default-mcp`;
2. each `--mcp-config FILE`, in order;
3. each `--mcp NAME=SPEC` on the command line.

The files use Claude Code's `.mcp.json` shape, so one file serves both:

```json
{
  "mcpServers": {
    "rea":  { "command": "npx", "args": ["-y", "rea-agents@latest", "mcp"],
              "env": { "REA_GHIDRA_STARTUP_TIMEOUT_MS": "120000" } },
    "docs": { "type": "http", "url": "https://example.com/mcp",
              "headers": { "Authorization": "Bearer ${DOCS_TOKEN}" } }
  }
}
```

`${VAR}` and `${VAR:-default}` expand from the environment. Keys operandi adds:

| key | meaning |
|-----|---------|
| `timeout` | seconds a call may take before it is cancelled (default 120) |
| `startupTimeout` | seconds to start + initialise + list tools (default 60; `npx -y` may download first) |
| `cwd` | working directory for a stdio server |
| `tools` | allow-list of the server's own tool names (`"inspect_*"` matches a prefix) |
| `workers` | `true` = `/ask` and Investigate workers may use every tool of this server. Otherwise they get only the tools the server marks `readOnlyHint`: workers are investigators, not fixers |
| `disabled` | `true` = skip this entry |

On the command line, `--mcp 'rea=npx -y rea-agents@latest mcp'` is a stdio server
(split like a shell would) and `--mcp docs=https://example.com/mcp` is an HTTP
one. `bin/operandi --mcp-status` lists what started, from where, and with which tools.

Things to know:

- **A project's own `.mcp.json` is not read automatically.** A cloned
  repository must not be able to make operandi launch a command. Pass it on
  purpose: `--mcp-config .mcp.json`.
- **Transports:** stdio, and Streamable HTTP (single JSON or SSE replies, a
  session id). The old HTTP+SSE transport is not supported.
- **Failures are not fatal.** A server that will not start, or dies, is
  reported (with its log, `~/.operandi/mcp-logs/<name>.log`, where its stderr
  goes) and its tools are left out. A config file or `--mcp` spec you named that
  cannot be read *is* fatal (exit 2).
- **Concurrency:** Fan and Investigate workers can call one server at once. A
  stdio connection routes each response to its request by id, so calls overlap
  instead of queuing.
- **Lifetime:** servers start before the first turn and are stopped when
  operandi exits. The ACP server (`bin/operandi-acp`) reads the default file too.
- **Offered capabilities:** tools only. operandi answers `ping`, and declines a
  server's requests for roots, sampling or elicitation.

`inspect/mcp-test.lisp` checks all of this against `mcp/test/fixture-server.mjs`
over both transports, with no model.

## MCP server

`mcp/server.js` runs the fast launcher per task and exposes, over stdio:

- `operandi_run`, `operandi_fan` and `operandi_swarm`, plus `operandi_get_run`,
  `operandi_cancel_run` and `operandi_swarm_status`;
- the question phase as data: `operandi_ask` (rival hypotheses → swarm
  verdicts → answer), `operandi_investigate` (the swarm on the caller's
  hypotheses) and `operandi_findings` (search the ledger, flagging records
  whose code has moved).

`OPERANDI_ROOT` points it at a different checkout (default: the repo root above
`mcp/`). See `mcp/README.md`.

The same question phase is on the command line as `bin/operandi --ask-json <
request.json`: a JSON request in, one `@@OPERANDI-JSON@@ {...}` line out
(`operandi.subagent:ask-structured` from Lisp).

```
cd mcp && npm install && node server.js
```

## Nostr DM agent (optional)

`operandi/nostr` is a separate ASDF system — a headless agent that chats with its
operator over **Nostr private DMs (NIP-17)**. The operator is identified by a
**NIP-05** address; only that cryptographically-authenticated key is answered,
and replies are gift-wrapped so only the operator can read them. It keeps the core
lean by pulling [cl-nostr](https://github.com/modus-lisp/cl-nostr) only for this
system.

```
OPERANDI_NOSTR_NIP05=you@example.com \
OPERANDI_NOSTR_MODEL=deepseek/deepseek-v4.1-flash \
  sbcl --non-interactive --load bin/operandi-nostr.lisp
```

It prints the agent's `npub` on startup — DM that from any NIP-17 client. Or drive
it from Lisp: `(asdf:load-system "operandi/nostr")` then
`(operandi.nostr:run-loop :nip05 "you@example.com")`. Offline oracle:
`nostr/nostr-test.lisp`.

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
| Default model | `~/.operandi/config.json` (`{"model": "..."}`) |
| OpenRouter token | `~/.operandi/openrouter.token` |
| Brave Search token | `BRAVE_API_KEY`, else `~/.operandi/brave-search.token` |
| Agent audit log DB | `~/.operandi/operandi.db` |
| Persistent agent notes | `~/.operandi/operandi-notes.md` |
| External MCP servers | `~/.operandi/mcp.json` (or `$OPERANDI_MCP_CONFIG`) |
| MCP server logs (stderr) | `~/.operandi/mcp-logs/` |
| TUI session transcripts | `~/.operandi/sessions/` |
| Compaction offload | `~/.operandi/offload/` (`turns-<YYYYMMDD-HHMMSS>-<NNNN>.txt`) |
| Cron reports / state | `~/.operandi/reports/`, `~/.operandi/cron.log` |

Tool-sandbox limits (rebindable specials in `operandi.tools`):
`*bash-timeout*` (120s — a Bash command is killed past this),
`*eval-timeout*` (60s — an Eval form is aborted past this).

## License

MIT

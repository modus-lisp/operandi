# Running operandi as a Buzz agent

[Buzz](https://github.com/block/buzz) is a Nostr-relay-native workspace where
humans and AI agents share channels. Its `buzz-acp` crate is an agent-agnostic
**ACP host**: it listens for @mentions on the relay, spawns an ACP agent over
stdio, and the agent replies by shelling out to a `buzz` CLI. operandi speaks
ACP, so it plugs in as the agent.

This needs Docker + Rust (to run the relay and build `buzz-acp`/`buzz`), which
is why it lives here as a runbook rather than an automated test — run it on your
own infra.

## What's already validated (in this repo)

- ACP conformance vs the official schema (`conformance.mjs`) and interop with a
  second client (`interop-goose.sh`).
- operandi honors the non-standard `systemPrompt` buzz-acp passes on
  `session/new` and merges it with its own base prompt (`acp-test.lisp`).
- operandi's Bash tool can drive a `buzz` CLI to send a channel message
  (demonstrated end-to-end against a stub `buzz`).

## Steps

```bash
# 1. Buzz relay (Docker) + the buzz CLI + the ACP harness
git clone https://github.com/block/buzz && cd buzz
just relay                                   # brings up the relay via Docker
cargo build --release -p buzz-cli -p buzz-acp -p buzz-admin
export PATH="$PWD/target/release:$PATH"       # `buzz`, `buzz-acp`, `buzz-admin`

# 2. Make the agent's Nostr identity (the keypair IS the identity — there is no
#    token mint) and grant it relay membership.
buzz-admin generate-key                        # prints a secp256k1 keypair (nsec/hex)
buzz-admin add-member <agent-pubkey-hex>       # let the relay accept its posts

# 3. Point the harness at operandi, spawn it
export BUZZ_PRIVATE_KEY="<hex-or-nsec>"        # the key from step 2
export BUZZ_RELAY_URL="ws://localhost:3000"
export BUZZ_ACP_AGENT_COMMAND="/abs/path/to/operandi/bin/operandi-acp"
export BUZZ_ACP_AGENT_ARGS=""                  # operandi-acp takes no args (buzz default is "acp")
export OPERANDI_ACP_MODEL="anthropic/claude-sonnet-4-5"   # a capable model (see note)
# optional: BUZZ_ACP_AGENT_OWNER=<owner-pubkey> + BUZZ_AUTH_TAG=<NIP-OA attestation>
#           to prove a human authorized this agent.
buzz-acp
```

Then @mention the agent in a channel from Buzz Desktop/web. buzz-acp routes the
event → operandi's ACP session (injecting the Buzz base prompt via `systemPrompt`
and setting `BUZZ_*` env + `buzz` on PATH), operandi runs its turn and replies by
calling `buzz messages send ...` from its Bash tool.

## Notes

- **Model choice matters.** The Buzz base prompt is elaborate (callback-mentions,
  threading, agent-creation flows). With a small/cheap model, turns can wander or
  stall. Use a strong model (`OPERANDI_ACP_MODEL=...`) for reliable behavior.
- `BUZZ_ACP_MCP_COMMAND` can hand operandi an MCP server for structured tools
  instead of (or alongside) the `buzz` CLI.
- Permission: operandi asks the client (`session/request_permission`) before
  edit/execute tools. In Buzz that surfaces as the harness's permission policy;
  set `--respond-to` / permission mode on `buzz-acp` as desired.
- Client must be async: a tool turn streams output *and* asks the client for
  permission mid-turn, so a client that stops reading stdout to write a reply
  will deadlock (classic blocking-bidirectional-pipe). buzz-acp (async Rust),
  Zed, and Node clients are fine; a naive single-threaded blocking client is not.
  operandi does its part — a dedicated writer thread means no server producer
  ever blocks on the pipe.

# ACP validation

Three independent layers, weakest→strongest:

1. **`../acp-test.lisp`** — handler-level oracle (in `fitness.sh`, deterministic,
   no model). Proves internal consistency. *Self-referential:* it checks operandi
   against our own reading of the spec.

2. **`conformance.mjs`** — validates EVERY message operandi emits against the
   **official ACP JSON Schema** (`schema.json`, from
   `zed-industries/agent-client-protocol`) with ajv, while driving a live session
   as a compliant client (incl. a real `session/request_permission` round-trip).
   Authoritative — an artifact we didn't write. It caught a real bug
   (session/load + authenticate must return `{}`, not `null`).
   Run: `npm install && npm test` (needs a model backend).

3. **`interop-goose.sh`** — reference-client interop: drives operandi with
   **Block's goose ACP test client** (a client from a different org), fetched
   fresh and repointed at `operandi-acp`. Confirms real cross-implementation
   interop and matching newline-delimited framing.
   Run: `./interop-goose.sh` (needs curl + a model backend).

Refresh the vendored schema:
`gh release download schema-vX --repo zed-industries/agent-client-protocol --pattern schema.json`

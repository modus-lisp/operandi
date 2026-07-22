// ACP conformance check for operandi's server.
//
// Validates EVERY JSON-RPC message operandi emits against the official ACP
// JSON Schema (schema.json, published by zed-industries/agent-client-protocol)
// while driving a full live session as a spec-compliant client — so it checks
// conformance against an artifact we did NOT write, and interop against a real
// client. Needs a model backend (OpenRouter token, or OPERANDI_ACP_MODEL).
//
//   cd inspect/acp && npm install && node conformance.mjs
//
// Exit 0 iff every emitted message validates; nonzero (with ajv errors) otherwise.

import Ajv2020 from "ajv/dist/2020.js";
import addFormats from "ajv-formats";
import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
import readline from "node:readline";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const REPO = path.resolve(HERE, "..", "..");

const schema = JSON.parse(readFileSync(path.join(HERE, "schema.json"), "utf8"));
schema.$id = "acp://schema";
const ajv = new Ajv2020({ strict: false, allErrors: true, validateFormats: false });
addFormats(ajv);
ajv.addSchema(schema);
const validatorFor = (def) => ajv.getSchema("acp://schema#/$defs/" + def);

const results = [];
function check(def, data, label) {
  const v = validatorFor(def);
  if (!v) { results.push({ ok: false, label, msg: `no schema def ${def}` }); return; }
  const ok = v(data);
  results.push({ ok, label: `${label}  (${def})`,
                 msg: ok ? "" : ajv.errorsText(v.errors, { separator: "; " }) });
}

// ---- drive the server ----
const srv = spawn("sbcl", ["--noinform", "--non-interactive", "--load", "bin/operandi-acp.lisp"],
                  { cwd: REPO, stdio: ["pipe", "pipe", "inherit"] });
const rl = readline.createInterface({ input: srv.stdout });
let nextId = 100;
const pending = new Map();
const seenUpdates = new Set();

function send(o) { srv.stdin.write(JSON.stringify(o) + "\n"); }
function request(method, params) {
  return new Promise((res) => { const id = nextId++; pending.set(id, res); send({ jsonrpc: "2.0", id, method, params }); });
}

rl.on("line", (line) => {
  line = line.trim(); if (!line) return;
  let m; try { m = JSON.parse(line); } catch { results.push({ ok: false, label: "wire", msg: "non-JSON line: " + line.slice(0, 80) }); return; }
  if (m.method === "session/update") {
    seenUpdates.add(m.params?.update?.sessionUpdate);
    check("SessionNotification", m.params, "notify session/update:" + m.params?.update?.sessionUpdate);
  } else if (m.method && m.id != null) {
    // an inbound REQUEST from the agent (permission / fs / terminal)
    if (m.method === "session/request_permission") {
      check("RequestPermissionRequest", m.params, "agent→client request_permission");
      send({ jsonrpc: "2.0", id: m.id, result: { outcome: "selected", optionId: m.params.options[0].optionId } });
    } else {
      send({ jsonrpc: "2.0", id: m.id, error: { code: -32601, message: "client does not support " + m.method } });
    }
  } else if (m.id != null && (m.result !== undefined || m.error !== undefined)) {
    const r = pending.get(m.id); pending.delete(m.id); if (r) r(m);
  }
});

const done = (code) => { try { srv.stdin.end(); } catch {} setTimeout(() => srv.kill(), 300); process.exitCode = code; };

try {
  const init = await request("initialize", { protocolVersion: 1,
    clientCapabilities: { fs: { readTextFile: false, writeTextFile: false }, terminal: false } });
  check("InitializeResponse", init.result, "response initialize");

  const ns = await request("session/new", { cwd: REPO, mcpServers: [] });
  check("NewSessionResponse", ns.result, "response session/new");
  const sid = ns.result?.sessionId;

  // a prompt that forces a tool call (execute kind -> a permission round-trip)
  const pr = await Promise.race([
    request("session/prompt", { sessionId: sid, prompt: [
      { type: "text", text: "Run this exact bash command: echo acp-conformance-ok . Then reply with only the word DONE." }] }),
    new Promise((_, rej) => setTimeout(() => rej(new Error("prompt timed out")), 60000)),
  ]);
  check("PromptResponse", pr.result, "response session/prompt");

  // resume the just-created session
  const ls = await request("session/load", { sessionId: sid, cwd: REPO, mcpServers: [] });
  if (ls.error) results.push({ ok: false, label: "response session/load", msg: JSON.stringify(ls.error) });
  else check("LoadSessionResponse", ls.result, "response session/load");

  // ---- report ----
  console.log("\nupdates seen:", [...seenUpdates].join(", ") || "(none)");
  let fails = 0;
  for (const r of results) {
    console.log(`  ${r.ok ? "ok  " : "FAIL"} ${r.label}${r.ok ? "" : "\n        " + r.msg}`);
    if (!r.ok) fails++;
  }
  // coverage: we want to have exercised the interesting update variants
  for (const need of ["agent_message_chunk", "tool_call", "tool_call_update"]) {
    if (!seenUpdates.has(need)) { console.log(`  warn never saw update: ${need} (model may not have used a tool)`); }
  }
  console.log(`\nconformance: ${results.length - fails}/${results.length} messages valid, ${fails} violation(s)`);
  done(fails === 0 ? 0 : 1);
} catch (e) {
  console.error("conformance error:", e.message);
  done(2);
}

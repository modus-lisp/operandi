#!/usr/bin/env bash
# Reference-client interop: drive operandi's ACP server with Block's goose ACP
# test client (an INDEPENDENTLY-written client, not ours). Fetches goose's
# test_acp_client.py fresh, repoints only its subprocess spawn at operandi-acp,
# and runs its full initialize / session-new / prompt / restart / session-load
# flow. Needs: curl, python3, sbcl, and a model backend (OpenRouter token).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
URL="https://raw.githubusercontent.com/block/goose/main/test_acp_client.py"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
echo "fetching Block's goose ACP client…" >&2
curl -fsSL "$URL" -o "$TMP/goose.py"
python3 - "$TMP/goose.py" "$TMP/client.py" <<'PY'
import sys
src=open(sys.argv[1]).read()
old="""        self.process = subprocess.Popen(
            ['cargo', 'run', '-p', 'goose-cli', '--', 'acp'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0
        )"""
new="""        import os
        self.process = subprocess.Popen(
            ['sbcl','--noinform','--non-interactive','--load','bin/operandi-acp.lisp'],
            cwd=os.environ['OPERANDI_ROOT'],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
            text=True, bufsize=0)"""
if old not in src: sys.exit("goose's client changed shape — update interop-goose.sh's patch")
open(sys.argv[2],"w").write(src.replace(old,new))
PY
OPERANDI_ROOT="$ROOT" exec python3 "$TMP/client.py"

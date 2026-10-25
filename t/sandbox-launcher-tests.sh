#!/usr/bin/env bash
# sandbox-launcher-tests.sh — the launcher's contract, checked without applying
# a sandbox (nested sandbox-exec is denied, so the profile is exercised by
# rendering it, not by running under it).
#
#   bash t/sandbox-launcher-tests.sh
#
# What it pins:
#   1. both dry-run modes print something — the bug this file was written for
#      was a launcher that exited 0 with no output at all;
#   2. a template with an unsubstituted __TOKEN__ fails loudly instead of
#      reaching sandbox-exec as literal text;
#   3. the project customization DSL reaches the profile, last;
#   4. the behaviour that must not change: the rendered local-projects rules are
#      byte-identical to what the bash script this replaced produced.
set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd -P)"
ROOT="$(cd "$HERE/.." && pwd -P)"
LAUNCHER="$ROOT/bin/operandi-sandbox-macos"
TMP="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/sandbox-launcher-tests.XXXXXX")" && pwd -P)"
trap 'rm -rf "$TMP"' EXIT

pass=0 fail=0
ok()   { pass=$((pass+1)); printf '  ok   %s\n' "$1"; }
bad()  { fail=$((fail+1)); printf '  FAIL %s\n' "$1"; }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1 (got '$2', want '$3')"; fi; }
count(){ printf '%s' "$1" | grep -c "$2"; }

printf '\n== sandbox launcher ==\n'

# --- 1. both dry-run modes produce output ---------------------------------
local_rules="$(cd "$ROOT" && OPERANDI_SANDBOX_DRYRUN=1 "$LAUNCHER" 2>/dev/null)"
check "DRYRUN prints the local-project rules" \
      "$(test -n "$local_rules" && echo yes || echo no)" yes
# One (allow-ancestor-metadata) + one (allow file-read*) pair per project.
n_pairs="$(count "$local_rules" 'allow file-read\* (subpath "')"
check "every rule is a full pair" \
      "$(count "$local_rules" 'allow-ancestor-metadata')" "$n_pairs"
check "there are projects to grant" "$(test "$n_pairs" -gt 0 && echo yes || echo no)" yes

profile="$(cd "$ROOT" && OPERANDI_SANDBOX_DRYRUN_PROFILE=1 "$LAUNCHER" 2>/dev/null)"
check "PROFILE prints a whole profile" "$(test -n "$profile" && echo yes || echo no)" yes
check "PROFILE is a version-1 SBPL profile" "$(count "$profile" '^(version 1)$')" 1
check "PROFILE denies by default" "$(count "$profile" '^(deny default)$')" 1
check "PROFILE carries the local-project rules" \
      "$(count "$profile" "$(printf '%s' "$local_rules" | head -1)")" 1

# --- 2. no unsubstituted token ever reaches sandbox-exec ------------------
check "no __TOKEN__ left in the profile" "$(count "$profile" '__[A-Z_]*__')" 0

mkdir -p "$TMP/fake/bin"
cp "$LAUNCHER" "$TMP/fake/bin/"
sed 's/^__EXTRA__$/__BOGUS_TOKEN__/' "$ROOT/bin/sandbox.sb" > "$TMP/fake/bin/sandbox.sb"
( cd "$TMP" && OPERANDI_SANDBOX_DRYRUN_PROFILE=1 "$TMP/fake/bin/operandi-sandbox-macos" ) \
  >"$TMP/fake.out" 2>"$TMP/fake.err"
check "a stray token is refused (exit 1)" "$?" 1
check "the stray token is named in the error" \
      "$(count "$(cat "$TMP/fake.err")" '__BOGUS_TOKEN__')" 1
check "nothing is printed to stdout on refusal" "$(wc -c <"$TMP/fake.out" | tr -d ' ')" 0

# --- 3. customization DSL -------------------------------------------------
mkdir -p "$TMP/proj/.operandi" "$TMP/proj/out"
cat > "$TMP/proj/.operandi/sandbox.lisp" <<'DSL'
(allow-read "~/definitely-not-a-real-dir-xyz")   ; dropped, with a warning
(allow-write "out")                              ; relative, delivered absolute
(allow-connect "localhost:11434")
(deny-connect "localhost:9931")
(raw "(allow mach-lookup (global-name \"com.apple.Foo\"))")
DSL
( cd "$TMP/proj" && OPERANDI_SANDBOX_DRYRUN_PROFILE=1 "$LAUNCHER" ) >"$TMP/p3" 2>"$TMP/e3"
check "raw rules reach the profile"            "$(count "$(cat "$TMP/p3")" 'com.apple.Foo')" 1
check "allow-write became file-read*+file-write*" \
      "$(count "$(cat "$TMP/p3")" "file-read\* file-write\* (subpath \"$TMP/proj/out\")")" 1
check "allow-connect became network-outbound" \
      "$(count "$(cat "$TMP/p3")" 'network-outbound (remote tcp "localhost:11434")')" 1
check "deny-connect stayed a deny" \
      "$(count "$(cat "$TMP/p3")" '^(deny network-outbound (remote tcp "localhost:9931"))$')" 1
check "a missing path is dropped with a warning" \
      "$(count "$(cat "$TMP/e3")" 'missing path: ~/definitely-not-a-real-dir-xyz')" 1
check "the user's rules are the last thing in the profile" \
      "$(tail -2 "$TMP/p3" | count "$(cat)" 'com.apple.Foo')" 1

# --- 4. the bash behaviour that must not change ---------------------------
mkdir -p "$TMP/bash/bin"
cp "$ROOT/bin/sandbox.sb" "$TMP/bash/bin/"
if git -C "$ROOT" show HEAD:bin/operandi-sandbox-macos > "$TMP/bash/bin/operandi-sandbox-macos" 2>/dev/null; then
  chmod +x "$TMP/bash/bin/operandi-sandbox-macos"
  ( cd "$ROOT" && OPERANDI_SANDBOX_DRYRUN=1 "$TMP/bash/bin/operandi-sandbox-macos" ) \
    >"$TMP/bash.local" 2>/dev/null
  test -s "$TMP/bash.local" || ( cd "$ROOT" && OPERANDI_SANDBOX_DRYRUN=1 bash "$TMP/bash/bin/operandi-sandbox-macos" ) >"$TMP/bash.local" 2>/dev/null
  ( cd "$ROOT" && OPERANDI_SANDBOX_DRYRUN=1 "$LAUNCHER" ) >"$TMP/lisp.local" 2>/dev/null
  if [ -s "$TMP/bash.local" ]; then
    if cmp -s "$TMP/bash.local" "$TMP/lisp.local"; then
      ok "local-project rules match the bash launcher byte for byte"
    else
      bad "local-project rules differ from the bash launcher"
      diff "$TMP/bash.local" "$TMP/lisp.local" | head -5
    fi
  else
    bad "could not run the old bash launcher for comparison"
  fi
else
  bad "could not read the old bash launcher from git"
fi

printf '\n%d passed, %d failed\n\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
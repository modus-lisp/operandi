#!/usr/bin/env bash
# fitness.sh <src-dir> — the held-out fitness function for self-improvement.
#
# Runs every canonical oracle suite (inspect/*-test.lisp in THIS repo) against
# the operandi checkout at <src-dir>, isolated so it tests THAT code and not
# canonical: CL_SOURCE_REGISTRY with :ignore-inherited-configuration is proven
# to win over the ~/quicklisp/local-projects symlink, so ql:quickload :operandi
# loads <src-dir>. The test FILES come from canonical (held out from whatever
# <src-dir> changed) — so a proposal can't game the fitness by editing its own
# copy of the tests. A single fresh ASDF cache is used for the whole run
# (compile <src-dir> once), never canonical's fasls.
#
# Exit 0 iff every suite passes.
set -u
SRC="$(cd "${1:?usage: fitness.sh <src-dir>}" && pwd)"
CANON="$(cd "$(dirname "$0")/.." && pwd)"
REG="(:source-registry (:tree \"$SRC\") :ignore-inherited-configuration)"
CACHE="$(mktemp -d)"
trap 'rm -rf "$CACHE"' EXIT

pass=0; fail=0; failed=""
for t in "$CANON"/inspect/*-test.lisp; do
  name="$(basename "$t" .lisp)"
  if CL_SOURCE_REGISTRY="$REG" XDG_CACHE_HOME="$CACHE" \
     timeout 240 sbcl --non-interactive --load "$t" >/dev/null 2>&1; then
    pass=$((pass+1))
  else
    fail=$((fail+1)); failed="$failed $name"
  fi
done
echo "fitness($(basename "$SRC")): $pass passed, $fail failed;$failed"
[ "$fail" -eq 0 ]

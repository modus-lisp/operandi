#!/usr/bin/env bash
# self-improve.sh — the propose -> snapshot -> held-out-fitness -> keep|revert
# loop that makes operandi self-modification SAFE and reversible.
#
#   self-improve.sh apply <patch-file>      # proposal = a diff to apply
#   self-improve.sh cmd '<shell command>'   # proposal = a command run in the worktree
#
# A proposal is applied in an ISOLATED git worktree branched from HEAD — the
# canonical tree is never touched. The held-out fitness (inspect/fitness.sh)
# runs the canonical oracle suites against the worktree's code. On PASS the
# proposal is committed on its own branch for you to review+merge; on FAIL it
# is discarded. Reversibility instead of prohibition: a bad self-edit can't
# survive, and a good one is a branch, not a fait accompli.
set -u
CANON="$(cd "$(dirname "$0")/.." && pwd)"
MODE="${1:?usage: self-improve.sh apply <patch> | cmd '<command>'}"
ARG="${2:?missing proposal}"

WT="$(mktemp -d)/wt"
BRANCH="self-improve-$$"
git -C "$CANON" worktree add -f -b "$BRANCH" "$WT" HEAD >/dev/null 2>&1 \
  || { echo "could not create worktree"; exit 3; }

revert() {
  git -C "$CANON" worktree remove --force "$WT" >/dev/null 2>&1
  git -C "$CANON" branch -D "$BRANCH" >/dev/null 2>&1
  git -C "$CANON" worktree prune >/dev/null 2>&1
}

echo "== proposal: applying in isolated worktree $WT (branch $BRANCH) =="
case "$MODE" in
  apply) git -C "$WT" apply "$ARG" || { echo "PATCH FAILED to apply"; revert; exit 2; } ;;
  cmd)   ( cd "$WT" && bash -c "$ARG" ) || { echo "PROPOSAL COMMAND FAILED"; revert; exit 2; } ;;
  *)     echo "unknown mode: $MODE"; revert; exit 2 ;;
esac

echo "== held-out fitness against the worktree =="
if "$CANON/inspect/fitness.sh" "$WT"; then
  git -C "$WT" add -A
  git -C "$WT" commit -q -m "self-improve: proposal (held-out fitness passed)" || true
  echo "KEEP — fitness passed. Proposal is on branch '$BRANCH'."
  echo "  review:  git -C $CANON diff master..$BRANCH"
  echo "  merge :  git -C $CANON merge $BRANCH && git -C $CANON worktree remove $WT"
  exit 0
else
  echo "REVERT — fitness regressed; discarding the proposal. Canonical is untouched."
  revert
  exit 1
fi

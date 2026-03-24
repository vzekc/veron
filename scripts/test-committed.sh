#!/usr/bin/env bash
# Test the committed state in an isolated worktree.
# This reproduces what CI sees — no uncommitted files, no stale FASLs.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORKTREE=""

cleanup() {
  if [ -n "$WORKTREE" ] && [ -d "$WORKTREE" ]; then
    cd "$REPO_ROOT"
    git worktree remove --force "$WORKTREE" 2>/dev/null || rm -rf "$WORKTREE"
  fi
}
trap cleanup EXIT

WORKTREE=$(mktemp -d "${TMPDIR:-/tmp}/veron-test.XXXXXX")
echo "=== Creating worktree at $WORKTREE ==="
git -C "$REPO_ROOT" worktree add --detach "$WORKTREE" HEAD
cd "$WORKTREE"

# Initialize submodules in worktree
git submodule update --init --recursive

# Clear ASDF cache for this path to force full recompilation
find "${HOME}/.cache/common-lisp" -path "*$(echo "$WORKTREE" | sed 's|/|*|g')*" -delete 2>/dev/null || true

echo "=== Running tests from committed state ==="
sbcl --non-interactive \
  --load "${HOME}/quicklisp/setup.lisp" \
  --load run-tests.lisp

echo "=== All tests passed ==="

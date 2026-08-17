#!/bin/bash
#
# Deploy VERON: pull latest code and hot-reload via Swank.
# Run as the veron user (e.g. via SSH forced command).
#
# Requires: SWANK_PORT, VERON_PORT in /etc/veron/env
# Optional: VERON_HOST, VERON_TLS_PORT
#
set -euo pipefail

VERON_DIR=/opt/veron

# Source environment (SWANK_PORT, VERON_PORT, VERON_HOST, etc.)
# /etc/veron/env is only present on the VM deployment; in the k3s
# container the same vars are supplied by the pod spec, so just skip.
if [ -f /etc/veron/env ]; then
  set -a
  source /etc/veron/env
  set +a
fi

echo "=== Pulling latest code ==="
cd "$VERON_DIR"

# GitHub's smart-HTTP occasionally returns a mid-protocol 500
# ("expected 'acknowledgments'") that clears within seconds. Retry the
# network git ops before giving up so a transient hiccup doesn't force
# the workflow's fallback path (hard pod restart).
retry() {
  local max=3 delay=2 n=0
  until "$@"; do
    n=$((n+1))
    if (( n >= max )); then
      echo "deploy.sh: giving up after $n attempts: $*" >&2
      return 1
    fi
    echo "deploy.sh: attempt $n failed, retrying in ${delay}s: $*" >&2
    sleep "$delay"
    delay=$((delay*2))
  done
}

retry git fetch origin
git reset --hard origin/main
# An already-initialised submodule keeps the URL it was cloned from in
# .git/config, so a submodule that has moved is picked up from .gitmodules here.
git submodule sync --recursive
retry git submodule update --init --recursive

echo "=== Hot-reloading via Swank (port ${SWANK_PORT}) ==="
SWANK_EVAL='(veron:reload)' \
  sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load deploy/swank-eval.lisp

echo "=== Running confidence test ==="
deploy/confidence-test.sh

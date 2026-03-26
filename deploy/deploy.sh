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
set -a
source /etc/veron/env
set +a

echo "=== Pulling latest code ==="
cd "$VERON_DIR"
git fetch origin
git reset --hard origin/main
git submodule update --init --recursive

echo "=== Hot-reloading via Swank (port ${SWANK_PORT}) ==="
SWANK_EVAL='(veron:reload)' \
  sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load deploy/swank-eval.lisp

echo "=== Running confidence test ==="
deploy/confidence-test.sh

#!/bin/bash
#
# Deploy VERON: pull latest code and restart the service.
# Run as the veron user (e.g. via SSH forced command).
#
set -euo pipefail

VERON_DIR=/opt/veron

echo "=== Pulling latest code ==="
cd "$VERON_DIR"
git pull --recurse-submodules

echo "=== Restarting service ==="
sudo /usr/bin/systemctl restart veron

echo "=== Waiting for service to start ==="
sleep 2
if systemctl is-active --quiet veron; then
    echo "VERON is running"
else
    echo "VERON failed to start, check: journalctl -u veron"
    exit 1
fi

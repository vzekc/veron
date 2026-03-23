#!/bin/bash
#
# Deploy VERON: pull latest code and restart the service.
# Run as root or via sudo.
#
set -euo pipefail

VERON_DIR=/opt/veron
VERON_USER=veron

echo "=== Pulling latest code ==="
cd "$VERON_DIR"
sudo -u "$VERON_USER" git pull --recurse-submodules

echo "=== Restarting service ==="
systemctl restart veron

echo "=== Waiting for service to start ==="
sleep 2
if systemctl is-active --quiet veron; then
    echo "VERON is running"
else
    echo "VERON failed to start, check: journalctl -u veron"
    exit 1
fi

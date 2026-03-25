#!/bin/bash
#
# Confidence test: verify VERON is serving the login screen.
# Tests both plain and TLS ports (if configured).
#
# Environment: VERON_HOST (default 127.0.0.1), VERON_PORT (default 3270),
#              VERON_TLS_PORT (optional, skip TLS test if unset)
#
set -euo pipefail

HOST=${VERON_HOST:-127.0.0.1}
PORT=${VERON_PORT:-3270}
TLS_PORT=${VERON_TLS_PORT:-}
PASSED=0
FAILED=0

check_login_screen() {
    local label=$1
    local s3270_args=$2

    echo "--- Testing $label ---"
    local result
    if ! result=$(printf 'Connect(%s)\nWait(10,3270Mode)\nWait(10,Unlock)\nAscii()\nDisconnect()\nQuit()\n' \
                         "$s3270_args" | s3270 -noverifycert 2>/dev/null); then
        echo "FAIL: s3270 exited with error"
        FAILED=$((FAILED + 1))
        return
    fi

    if echo "$result" | grep -q "Benutzername"; then
        echo "PASS: login screen detected"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL: login screen not found"
        echo "Screen output:"
        echo "$result"
        FAILED=$((FAILED + 1))
    fi
}

# Test plain port
check_login_screen "plain port ($HOST:$PORT)" "$HOST:$PORT"

# Test TLS port if configured
if [ -n "$TLS_PORT" ]; then
    check_login_screen "TLS port ($HOST:$TLS_PORT)" "L:$HOST:$TLS_PORT"
fi

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="

if [ "$FAILED" -gt 0 ]; then
    exit 1
fi

#!/bin/bash
#
# One-time setup for VERON on a Debian deployment target.
# Run as root.
#
set -euo pipefail

VERON_USER=veron
VERON_DIR=/opt/veron
REPO_URL=https://github.com/vzekc/veron.git
ENV_FILE=/etc/veron/env

echo "=== Installing system packages ==="
apt-get update
apt-get install -y sbcl postgresql git

echo "=== Creating veron user ==="
if ! id "$VERON_USER" &>/dev/null; then
    useradd --system --create-home --shell /bin/bash "$VERON_USER"
fi

echo "=== Setting up PostgreSQL ==="
if ! sudo -u postgres psql -tc "SELECT 1 FROM pg_roles WHERE rolname='$VERON_USER'" | grep -q 1; then
    sudo -u postgres createuser "$VERON_USER"
fi
if ! sudo -u postgres psql -tc "SELECT 1 FROM pg_database WHERE datname='veron'" | grep -q 1; then
    sudo -u postgres createdb -O "$VERON_USER" veron
fi

echo "=== Cloning repository ==="
if [ ! -d "$VERON_DIR" ]; then
    git clone --recurse-submodules "$REPO_URL" "$VERON_DIR"
    chown -R "$VERON_USER:$VERON_USER" "$VERON_DIR"
else
    echo "$VERON_DIR already exists, skipping clone"
fi

echo "=== Installing Quicklisp ==="
if [ ! -f "/home/$VERON_USER/quicklisp/setup.lisp" ]; then
    sudo -u "$VERON_USER" bash -c '
        curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
        sbcl --non-interactive \
            --load /tmp/quicklisp.lisp \
            --eval "(quicklisp-quickstart:install)" \
            --eval "(ql-util:without-prompting (ql:add-to-init-file))"
    '
fi

echo "=== Preloading Quicklisp dependencies ==="
sudo -u "$VERON_USER" bash -c "
    cd $VERON_DIR
    sbcl --non-interactive \
        --load /home/$VERON_USER/quicklisp/setup.lisp \
        --load load.lisp \
        --eval '(uiop:quit)'
"

echo "=== Creating environment file ==="
mkdir -p /etc/veron
if [ ! -f "$ENV_FILE" ]; then
    cat > "$ENV_FILE" << 'ENVEOF'
VERON_DB_HOST=127.0.0.1
VERON_DB_PORT=5432
VERON_DB_NAME=veron
VERON_DB_USER=veron
VERON_DB_PASSWORD=
ENVEOF
    echo "Created $ENV_FILE — edit it to set passwords and optional auth DB settings"
else
    echo "$ENV_FILE already exists, not overwriting"
fi
chown root:$VERON_USER "$ENV_FILE"
chmod 640 "$ENV_FILE"

echo "=== Installing systemd service ==="
cp "$VERON_DIR/deploy/veron.service" /etc/systemd/system/
systemctl daemon-reload
systemctl enable veron

echo ""
echo "Setup complete. Next steps:"
echo "  1. Edit $ENV_FILE with database passwords"
echo "  2. systemctl start veron"
echo "  3. Verify: telnet localhost 3270"

# veron

V.z.E.k.C. Electronic Retro Online Network — a Common Lisp TN3270
application. Authenticates against WoltLab forum user accounts, serves
text-mode screens over 3270 (optionally STARTTLS-upgraded).

## Quick reference

| Thing                     | Where                                                         |
| ------------------------- | ------------------------------------------------------------- |
| Entry point               | `src/veron.lisp` → `(veron:start-from-env)`                   |
| Build system              | ASDF (`veron.asd`); uses Quicklisp                            |
| Framework                 | [LISPF](lispf/) (submodule)                                   |
| Auth backend              | [woltlab-login](woltlab-login/) — queries WoltLab's MySQL DB  |
| Port                      | 3270 (configurable; STARTTLS default when TLS cert provided)  |
| Live deployment           | systemd on retrostar, reachable at `veron.classic-computing.de:3270` |
| Cluster deployment        | `dev` namespace in `vzekc-prod`, reachable at `veron.k8s.classic-computing.de:3270` |
| Operator guide            | See [cluster-infra/README.md](https://github.com/vzekc/cluster-infra/blob/main/README.md) |
| Container image           | `ghcr.io/vzekc/veron:main` (built on every push to `main`)    |

## Running locally

Existing VM / dev workflow — install SBCL, Quicklisp, the submodules, set
up a local Postgres, put config in `.env`:

```bash
git submodule update --init --recursive
createdb veron
sbcl --load /path/to/quicklisp/setup.lisp --load load.lisp \
     --eval '(veron:start-from-env)'
```

`.env` / env variables:

- `VERON_DB_HOST` / `_PORT` / `_NAME` / `_USER` / `_PASSWORD` — Postgres for veron data
- `VERON_AUTH_DB_*` — WoltLab MySQL for user auth (falls back to `VERON_DB_*` if unset)
- `VERON_HOST` (`0.0.0.0`) / `VERON_PORT` (`3270`) — bind address
- `VERON_TLS_CERT` / `VERON_TLS_KEY` — enables STARTTLS (default on when cert provided)
- `VERON_NATS_URL` — optional; publishes status events to NATS
- `SWANK_PORT` — optional; start a Swank server on this port (default bind `127.0.0.1`)
- `SWANK_HOST` — override Swank's bind address if you need to expose it (not recommended)
- `EXHIBITRON_DB_*` — optional integration with the exhibitron DB
- `VERON_PRINT_PORT` / `VERON_PRINT_TOKEN` — enables Fotodruck (see below)
- `VERON_FOTOFIX_URL` (`https://2026.classic-computing.de`) — where print runs are fetched

Full set documented in the docstring on `veron:start-from-env` in `src/veron.lisp`.

## Fotodruck

The Fotodruck screen prints a visitor's photo from the fotofix booth on a
dot-matrix printer at the show. The user types the photo id from their receipt
and picks a density; veron fetches that photo's print run from the exhibition
website and sends it to the printer.

The printer hangs off a print server on the show's internal network, which has
no inbound route, so a relay beside it dials in to `VERON_PRINT_PORT` and holds
the connection open. A relay connected is what makes the screen offer the form;
with none connected it says so and does not ask for an id. One sheet at a time:
a second user meanwhile is told the printer is busy and roughly how long is
left.

The bytes go out on the schedule the chosen density prints at, so what is left
in the buffers between veron and the paper stays small: the run ends when the
sheet does, and how much has gone out is a fair measure of how far the sheet is.
A printer slower than that schedule holds the writes back and the figures follow
it. The relay's connection ends with the run and is dialled again, so a printer
that has just printed reads as busy until it is back.

What the visitor is told about the wait is kept to what is known. Nothing is
claimed for the first `*estimate-warmup-seconds*`, where too little has gone out
to judge the pace; after that the figure is rounded up to a quarter minute so it
does not read as exact, and it carries `*print-drain-seconds*` for the bytes
still between veron and the paper. The run waits that much out after its last
write, reporting the sheet as nearly done, and only then as finished.

The relay signs on with one line — the printer's name and the token — and
everything after it travels the other way:

```
relay -> veron:  "nec-p6 <VERON_PRINT_TOKEN>\n"
veron -> relay:  the bytes of the print run
veron -> relay:  close, which is the end of the run
```

The relay lives with the show's other kit in the
[fotofix](https://code.netzhansa.com/hanshuebner/fotofix) repository
(`print-relay/`, installed by `ansible/roles/print-relay/`). It dials
`veron.classic-computing.de:3272` on retrostar — 3271 there is the TLS 3270
port. The protocol is small enough to speak from a shell, which is how to try
it by hand:

```sh
{ printf 'nec-p6 %s\n' "$TOKEN"; sleep 999999; } \
  | nc veron.classic-computing.de 3272 \
  | nc 192.168.2.222 3002
```

The `sleep` holds the sending direction open; a relay whose socket half-closes
is one veron reads as having gone away.

Printers are a table in `src/print.lisp` — name, the file-name stem on the
website, and the densities offered with their printing times. Adding a printer
is another entry there; when more than one relay is connected the screen asks
which printer to use.

Turning it on for a show is `VERON_PRINT_PORT=3272` and a `VERON_PRINT_TOKEN`
in `/etc/veron/env`, and the same token in `/etc/fotofix-print-relay.env` on the
exhibition Pi. The env file is read when the process starts, so this needs
`systemctl restart veron` rather than a hot reload. With either variable unset
nothing listens and the screen reports Fotodruck as unconfigured.

## Deploying

The install visitors reach is on **retrostar**
(`veron.classic-computing.de`, 3270 and 3271 for TLS), run by systemd from
`/opt/veron`. A push to `main` that passes CI triggers
`.github/workflows/deploy.yml`, which ssh's in as `veron@retrostar` and runs
`/opt/veron/deploy/deploy.sh` — the same fetch-and-hot-reload described below.
Env lives in `/etc/veron/env` and is read at process start, so a change there
needs `systemctl restart veron`.

A second copy runs in the vzekc k3s cluster at
`veron.k8s.classic-computing.de:3270`, deployed by its own flow:

1. `git push` to `main`.
2. GitHub Actions (`.github/workflows/image.yml`) builds + publishes
   `ghcr.io/vzekc/veron:{main, latest, sha-<short>}`.
3. The same workflow's `deploy:` job runs a **soft-deploy**:
   `kubectl exec` into the running pod and invokes
   `/opt/veron/deploy/deploy.sh`, which does:
   - `git fetch origin main && git reset --hard origin/main`
   - `git submodule update --init --recursive`
   - Invokes `(veron:reload)` via Swank — hot-reloads the ASDF systems
     in place, **without dropping existing 3270 sessions**.
   - Runs `deploy/confidence-test.sh` (an `s3270`-driven check that the
     login screen is still served).
4. If `deploy.sh` exits non-zero, the workflow falls back to
   `kubectl rollout restart deployment/veron`, which pulls the freshly
   pushed `:main` image and creates a new pod (drops existing
   sessions, but guarantees a clean restart).

The same flow is available manually from a laptop:
```bash
cluster-infra/scripts/soft-deploy-veron.sh
```

See [cluster-infra's operator runbook](https://github.com/vzekc/cluster-infra/blob/main/README.md)
for the deploy-job RBAC + `KUBECONFIG_VERON_DEPLOY` GH secret wiring.

## Connecting to the Swank REPL in the cluster

Swank binds only to the pod's loopback (not the pod network, not the
internet). Reach it via `kubectl port-forward`, which runs through the
k3s apiserver and is authenticated by your kubeconfig.

**Prereqs:**
- `kubectl` ≥ 1.32
- Your `~/.kube/vzekc-prod.config` (in `vzekc/cluster-infra` operator
  workflow; use `make kubeconfig` there to fetch).
- **Remove `~/.slime-secret` from your laptop** if you ever had one.
  SLIME sends the secret as its first packet and the swank shipped in
  Quicklisp mis-parses it (it expects a Lisp-quoted string; SLIME
  sends a bare identifier), which stalls the handshake.

**Open the tunnel** (leave running in a terminal):
```bash
kubectl --kubeconfig ~/.kube/vzekc-prod.config \
        port-forward -n dev deploy/veron 4005:4005
```

You'll see `Forwarding from 127.0.0.1:4005 -> 4005` and a
`Handling connection for 4005` line each time a client connects.

**Connect from Emacs:**
```
M-x slime-connect RET 127.0.0.1 RET 4005 RET
```

You now have a REPL in the running SBCL of the deployed veron pod.
Every definition you `C-c C-c` (compile-defun) or `C-c C-k` (compile-
file) is live in the process — useful for poking at state or
prototyping. Remember:

- The pod's `/opt/veron/` is a *git checkout*. Anything you redefine
  from SLIME lives only in memory; next soft-deploy resets it to
  `origin/main`. Next image pull resets it to the image's baked-in
  code.
- If you accidentally break something badly (e.g. crash the accept
  loop), liveness probes will restart the pod on the next failed
  check. Fresh image, clean slate.
- Swank accepts arbitrary Lisp — it's root-equivalent inside the pod
  (running as uid 10001, but can hit the Postgres cluster, NATS, etc.
  with the pod's credentials). Treat a connected REPL with the same
  care you'd treat a production SSH session.

## Debugging + ops references

- **Image source repo layout:** `Dockerfile` (multi-stage; base is
  `debian:12-slim` + sbcl + libvips/libpq/libmariadb3 + Quicklisp
  pre-installed; sources copied with `.git` intact so the runtime can
  `git fetch`). `.dockerignore` intentionally keeps `.git` — don't
  remove that.
- **Logs (Grafana Cloud Loki):** filter by `service_name="veron"` (set
  via a pod annotation; CNPG Postgres is `service_name="veron-db"`).
- **Metrics (Grafana Cloud Prometheus):** node + kube-state + pod
  metrics via Grafana Alloy.
- **Health checks:** readiness + liveness probe `tcpSocket:3270`.
  LISPF silently swallows stream errors before TN3270 negotiation, so
  probe + Hetzner-LB health-check connects don't spam the log.
- **Hetzner LB** terminates nothing for 3270 — it's a TCP passthrough
  on listen_port 3270 → NodePort 32270 → pod :3270. STARTTLS is
  negotiated end-to-end between client and veron.

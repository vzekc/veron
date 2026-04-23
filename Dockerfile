# Mirrors the prod systemd setup (deploy/setup.sh + deploy/veron.service):
# debian-slim + sbcl + Quicklisp + veron sources + pre-warmed FASL cache.
FROM debian:12-slim

# Layer 1: system packages. Split so apt state doesn't bust build cache on
# source changes.
RUN apt-get update && apt-get install -y --no-install-recommends \
      sbcl \
      git \
      ca-certificates \
      curl \
      libpq5 \
      libssl3 \
      libmariadb3 \
    && rm -rf /var/lib/apt/lists/*

# woltlab-login depends on cl-mysql, which dlopens libmysqlclient_r /
# libmysqlclient by name at system-load time. libmariadb3 is ABI-
# compatible but installs as libmariadb.so.3. Symlink the legacy names
# so CFFI finds the library.
RUN ln -sf libmariadb.so.3 /usr/lib/x86_64-linux-gnu/libmysqlclient.so \
 && ln -sf libmariadb.so.3 /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so \
 && ldconfig

# Layer 2: dedicated non-root user. UID matches the Deployment's
# securityContext.runAsUser in the cluster manifests.
RUN useradd --uid 10001 --create-home --shell /bin/bash veron
USER veron
WORKDIR /home/veron

# Layer 3: Quicklisp. Separate RUN so cache survives source churn.
RUN curl -fsSLo /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/veron/quicklisp/")' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
 && rm /tmp/quicklisp.lisp

# Layer 4: sources. Submodules are checked out at build time by the CI
# workflow (actions/checkout with submodules:recursive).
COPY --chown=veron:veron . /opt/veron
WORKDIR /opt/veron

# Layer 5: pre-load so Quicklisp deps are fetched and FASLs cached. The
# container starts cold in <3s instead of >30s on first run.
RUN sbcl --non-interactive \
         --load /home/veron/quicklisp/setup.lisp \
         --load load.lisp \
         --eval '(uiop:quit)'

EXPOSE 3270

# Defaults mirror what start-from-env picks up when env is unset.
ENV VERON_HOST=0.0.0.0 \
    VERON_PORT=3270

# Mirror deploy/veron.service exactly. start-from-env blocks on the 3270
# accept loop, so SBCL doesn't exit under --non-interactive.
ENTRYPOINT ["sbcl", "--non-interactive", \
            "--load", "/home/veron/quicklisp/setup.lisp", \
            "--load", "load.lisp", \
            "--eval", "(veron:start-from-env)"]

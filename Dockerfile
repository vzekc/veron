# Mirrors the prod systemd setup (deploy/setup.sh + deploy/veron.service):
# debian-slim + sbcl + Quicklisp + veron sources + pre-warmed FASL cache.
#
# Kept intentionally as a *mutable-in-place* image: /opt/veron is a real
# git checkout, so deploy/deploy.sh can do `git fetch && git reset ...`
# inside the running pod and hot-reload via Swank without a pod restart.
# If hot-reload breaks the 3270 listener, the liveness probe pulls a
# fresh pod from this image anyway — that's the failsafe.
FROM debian:12-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
      sbcl \
      git \
      ca-certificates \
      curl \
      libpq5 \
      libssl3 \
      libmariadb3 \
      x3270 \
    && rm -rf /var/lib/apt/lists/*
# x3270 provides s3270, used by deploy/confidence-test.sh after a
# soft-deploy to verify the 3270 listener still serves the login screen.

# woltlab-login depends on cl-mysql, which dlopens libmysqlclient_r /
# libmysqlclient by name at system-load time. libmariadb3 is ABI-
# compatible but installs as libmariadb.so.3. Symlink the legacy names
# so CFFI finds the library.
RUN ln -sf libmariadb.so.3 /usr/lib/x86_64-linux-gnu/libmysqlclient.so \
 && ln -sf libmariadb.so.3 /usr/lib/x86_64-linux-gnu/libmysqlclient_r.so \
 && ldconfig

# Dedicated non-root user. UID matches the Deployment's runAsUser.
RUN useradd --uid 10001 --create-home --shell /bin/bash veron
USER veron
WORKDIR /home/veron

RUN curl -fsSLo /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
         --load /tmp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install :path "/home/veron/quicklisp/")' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
 && rm /tmp/quicklisp.lisp

# Sources + .git. The CI workflow sets fetch-depth: 0 so the baked-in
# history is complete enough for `git fetch origin main` at runtime.
# Submodules are checked out by actions/checkout with submodules:recursive.
COPY --chown=veron:veron . /opt/veron
WORKDIR /opt/veron

# Prime the FASL cache so cold pod start is ~3s instead of ~30s.
RUN sbcl --non-interactive \
         --load /home/veron/quicklisp/setup.lisp \
         --load load.lisp \
         --eval '(uiop:quit)'

EXPOSE 3270 4005

ENV VERON_HOST=0.0.0.0 \
    VERON_PORT=3270 \
    SWANK_PORT=4005

# Matches deploy/veron.service. start-from-env blocks on the 3270
# accept loop so SBCL doesn't exit under --non-interactive.
ENTRYPOINT ["sbcl", "--non-interactive", \
            "--load", "/home/veron/quicklisp/setup.lisp", \
            "--load", "load.lisp", \
            "--eval", "(veron:start-from-env)"]

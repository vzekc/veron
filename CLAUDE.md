# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VERON (V.z.E.k.C. Electronic Retro Online Network) is a Common Lisp 3270 terminal application for the V.z.E.k.C. (Verein zum Erhalt klassischer Computer e.V.). It is built on top of the LISPF framework and uses WoltLab forum authentication.

## Build and Load Commands

**Loading the application** (in a running SBCL with Quicklisp):
```lisp
(asdf:load-system :veron)
```

**Starting the server:**
```lisp
(veron:start :port 3270 :host "127.0.0.1")
```

Environment variables must be set (or present in `.env`): `VERON_DB_HOST`, `VERON_DB_PORT`, `VERON_DB_NAME`, `VERON_DB_USER`, `VERON_DB_PASSWORD`, `VERON_AUTH_DB_HOST`, `VERON_AUTH_DB_PORT`, `VERON_AUTH_DB_NAME`, `VERON_AUTH_DB_USER`, `VERON_AUTH_DB_PASSWORD`. Optional: `VERON_ADMIN_GROUPS`. For Classic Computing integration: `EXHIBITRON_DB_NAME` (required, if not set the CC menu shows "not configured"), `EXHIBITRON_DB_USER`, `EXHIBITRON_DB_PASSWORD`, `EXHIBITRON_DB_HOST`, `EXHIBITRON_DB_PORT` (these fall back to `VERON_DB_*` values).

## Architecture

### ASDF System

- **veron** (`veron.asd`) - Main application, depends on `lispf`, `lispf-edit`, `woltlab-login`, `postmodern`

### Source Files (`src/`)

- `package.lisp` - Package definition with local nicknames (`pomo`, `editor`)
- `dotenv.lisp` - `.env` file loader
- `db.lisp` - PostgreSQL connection, migration runner, persistence functions (guestbook, login log)
- `user.lisp` - User class, admin group checking, `make-user` from auth result
- `files.lisp` - File management library: DB CRUD, mime type handling, binary encoding/decoding, filesystem bridge for editor integration
- `veron.lisp` - Application definition, session class, screen updates, key handlers, server entry point

### Screen Files (`screens/`)

Screen definitions use s-expression format (`.screen` and `.menu` files). The UI is in German.

**Screen row numbering:** The `:screen` string starts with a newline after the opening `"`. The framework adds a header row (screen name, title, timestamp) at physical row 0 and renders the screen content starting at physical row 1. Field `:from` coordinates are relative to the screen content (row 0 = first line after the opening newline of the `:screen` string). Count rows carefully — the leading newline after `"` is consumed, so the first visible line of text is row 0 in the content but row 1 on the physical display.

**Subapplication handover screens:** To invoke a subapplication (editor, help viewer) from a menu item, a minimal `.screen` file is required as a handover. The framework must load and register the screen before it can transition to it and call `prepare-screen`. The `.screen` file is minimal (just a name and empty content). The `define-screen-update` for that screen calls the subapplication (which blocks), then returns `:back` when done. Examples: `notes.screen` invokes the editor via `edit-file`; `hilfe.screen` invokes the help viewer via `show-help`.

### Database

PostgreSQL with sequential migrations in `migrations/`. The migration runner in `db.lisp` tracks applied versions in a `schema_migrations` table.

Tables: `users`, `logins`, `guestbook`, `files` (with BYTEA content and mime_type).

*NEVER* edit migration files as they could already have been applied.  Create new migrations as needed.

### Submodules

- `lispf/` - LISPF framework (screen registry, session management, key dispatch, editor)
- `woltlab-login/` - WoltLab forum authentication
- `CL3270/` - 3270 terminal protocol library (nested under lispf)

## Key Conventions for Lisp Code

- **defclass over defstruct** always — never use defstruct
- **case with find-symbol** for command dispatch instead of cond chains
- **Early returns** with `unless`/`when` + `return-from` instead of `(if cond (progn ...) error-value)`
- **unless over negated if**: `(unless cond ...)` instead of `(if cond nil ...)` — avoids progn in the else branch
- **Combined setf**: `(setf a 1 b 2 c 3)` instead of separate setf calls
- **when-let** (from alexandria) for bind-and-test patterns
- **Top-level form separation**: Top level forms and comment blocks are separated by empty lines

## CI and Testing

**Running a single test:** Use `load-tests.lisp` (loads without running), then `run-tests`:
```bash
sbcl --non-interactive --load ~/quicklisp/setup.lisp --load load-tests.lisp \
  --eval '(lispf-test:run-tests (quote test-name) :package :package-name)'
```
Examples:
- `(lispf-test:run-tests 'e2e-confirmation-confirm-logout :package :veron-tests)`
- `(lispf-test:run-tests 'layout-default-page-size :package :lispf-editor-tests)`

Always run individual tests during development. Run the full suite before committing.

**Verifying before push:** Run `scripts/test-committed.sh` to test the committed state in an isolated git worktree. This reproduces exactly what CI sees — no uncommitted files, no stale ASDF cache. Always do this before pushing, especially after cross-submodule changes.

**Submodule changes must be atomic:** When modifying code in a submodule (lispf, woltlab-login) that the parent repo depends on, commit and push the submodule first, then update the submodule pointer in veron and push. Never commit parent code that references symbols/functions only present in uncommitted submodule changes.

**Do not trust eval_swank for CI validation:** The running Lisp image has stale definitions from previous loads. Code that works via eval_swank may fail on a clean build if files aren't committed or load order is wrong.

**ASDF picks up untracked files:** If an uncommitted `.asd` file or source file exists locally, ASDF will find and load it. CI won't have it. The worktree test script catches this.

**Automated 3270 login:** When using the tn3270 tool to reproduce a system behavior outside of the login area, fill in the login screen with a sequence of String(<username>)Tab()String(<password>)Enter()Enter() to speed up the process and skip over the news page.

## MCP Integration

The lisp-mcp tool provides `eval_swank` and `eval_host_cl` for evaluating Lisp expressions in a running image. The parameter name is `expression` (not `code`). Use `edit_lisp` for paredit-safe structural editing of Lisp source files.

The tn3270 tool can be used to veron running on localhost in order to reproduce problems reported by the user.  The user "klaus" with password "klaus" is usually available, but ask the user for credentials if you have trouble logging in.

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

Environment variables must be set (or present in `.env`): `VERON_DB_HOST`, `VERON_DB_PORT`, `VERON_DB_NAME`, `VERON_DB_USER`, `VERON_DB_PASSWORD`, `VERON_AUTH_DB_HOST`, `VERON_AUTH_DB_PORT`, `VERON_AUTH_DB_NAME`, `VERON_AUTH_DB_USER`, `VERON_AUTH_DB_PASSWORD`. Optional: `VERON_ADMIN_GROUPS`.

## Architecture

### ASDF System

- **veron** (`veron.asd`) - Main application, depends on `lispf`, `lispf-edit`, `woltlab-login`, `postmodern`

### Source Files (`src/`)

- `package.lisp` - Package definition with local nicknames (`lspf`, `wl`, `pomo`, `editor`)
- `dotenv.lisp` - `.env` file loader
- `db.lisp` - PostgreSQL connection, migration runner, persistence functions (guestbook, login log)
- `user.lisp` - User class, admin group checking, `make-user` from auth result
- `files.lisp` - File management library: DB CRUD, mime type handling, binary encoding/decoding, filesystem bridge for editor integration
- `veron.lisp` - Application definition, session class, screen updates, key handlers, server entry point

### Screen Files (`screens/`)

Screen definitions use s-expression format (`.screen` and `.menu` files). The UI is in German.

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
- **Combined setf**: `(setf a 1 b 2 c 3)` instead of separate setf calls
- **when-let** (from alexandria) for bind-and-test patterns
- **Top-level form separation**: Top level forms and comment blocks are separated by empty lines

## MCP Integration

The lisp-mcp tool provides `eval_swank` and `eval_host_cl` for evaluating Lisp expressions in a running image. The parameter name is `expression` (not `code`). Use `edit_lisp` for paredit-safe structural editing of Lisp source files.

The tn3270 tool can be used to veron running on localhost in order to reproduce problems reported by the user.  The user "klaus" with password "klaus" is usually available, but ask the user for credentials if you have trouble logging in.

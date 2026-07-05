# Agent Instructions

This repo is a Haskell library for SendGrid v3 Mail Send.

## Rules

- Do not merge PRs.
- Do not push to default branches.
- Keep changes focused.
- Do not add broad cleanup.
- Do not invent SendGrid behavior. Check current SendGrid docs when behavior is unclear.
- Keep examples on v3 API keys and Bearer auth.

## Before Editing

1. Read `sendgrid-haskell.cabal`.
2. Read `README.md`.
3. Read `.factory/STANDARDS.md`.
4. Inspect the owning module before editing.

## Verification

Run the best available checks:

```sh
cabal build all
cabal test all
cabal check
```

If local Haskell tools are missing, add or update CI and state exactly what could not be run.

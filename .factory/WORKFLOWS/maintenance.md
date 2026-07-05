# Maintenance Workflow

Use this workflow for routine package maintenance.

## Steps

1. Read `sendgrid-haskell.cabal`, `README.md`, and `.factory/STANDARDS.md`.
2. Identify one focused change.
3. Add or update tests for the changed behavior.
4. Run:

   ```sh
   cabal build all
   cabal test all
   cabal check
   ```

5. Update README or release docs when public behavior changes.
6. Add a short entry to `.factory/JOURNAL.md` when the change is meaningful.

## Stop Conditions

- Stop before merging.
- Stop before releases.
- Stop if SendGrid API behavior is unclear and no source has been checked.

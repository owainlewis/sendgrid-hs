# Standards Check Workflow

Use this workflow to keep the project ready for humans and agents.

## Steps

1. Read `.factory/STANDARDS.md`, `.factory/AGENTS.md`, `README.md`, and `sendgrid-haskell.cabal`.
2. Check that README examples use SendGrid v3 API keys and Bearer auth.
3. Check that the package still builds and tests:

   ```sh
   cabal build all
   cabal test all
   cabal check
   ```

4. Check that `.github/workflows/ci.yml` runs build, test, and package checks.
5. Update `.factory/JOURNAL.md` when the standards change.

## Stop Conditions

- Stop before merging.
- Stop before releases.
- Stop if SendGrid API behavior is unclear and no current source has been checked.

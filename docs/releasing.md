# Releasing

This project uses Cabal package versions.

## Checklist

1. Confirm `CHANGELOG.md` describes the release.
2. Run the full local verification:

   ```sh
   cabal build all
   cabal test all
   cabal check
   ```

3. Confirm GitHub Actions passes on the release branch.
4. Update the version in `sendgrid-haskell.cabal` if needed.
5. Create a signed tag:

   ```sh
   git tag -s v3.0.0.0 -m "v3.0.0.0"
   ```

6. Upload the package only after human review.

## Compatibility

Version `3.0.0.0` is a breaking change from the old v2 client. It uses API keys, Bearer auth, and JSON for SendGrid v3 Mail Send.

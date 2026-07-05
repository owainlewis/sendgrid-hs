# Standards

## Product

- This package is a SendGrid v3 Mail Send client.
- Prefer typed request, response, and error values.
- Keep the public API small and explicit.
- Do not reintroduce SendGrid v2 username/password auth.

## Code

- Keep modules simple.
- Use explicit exports.
- Prefer total helpers and clear error paths.
- Hide API keys in `Show` output.
- Keep JSON field names aligned with SendGrid v3 Mail Send.

## Tests

- Cover JSON encoding for public request types.
- Cover request construction, including method, path, auth, headers, and body.
- Cover parsing for SendGrid error responses.
- Run:

  ```sh
  cabal build all
  cabal test all
  cabal check
  ```

## Documentation

- README examples must use API keys and v3 Mail Send.
- Release notes must call out breaking API changes.

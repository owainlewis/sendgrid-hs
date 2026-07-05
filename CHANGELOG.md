# Changelog

All notable changes to this project are documented here.

## 3.0.0.0

- Replaced the legacy SendGrid v2 username/password API with SendGrid v3 Mail Send.
- Added Bearer token authentication with JSON request bodies.
- Added typed request values for personalizations, content, attachments, categories, custom args, templates, ASM, mail settings, and tracking settings.
- Added typed response and error values.
- Added tests for JSON encoding, request construction, and error response parsing.
- Modernized Cabal metadata and CI.

## 1.0.0.0

- Initial legacy SendGrid v2 client.

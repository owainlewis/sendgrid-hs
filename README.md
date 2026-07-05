# sendgrid-haskell

A typed Haskell client for SendGrid v3 Mail Send.

This package sends JSON requests to `POST https://api.sendgrid.com/v3/mail/send` with `Authorization: Bearer <API key>`.

## Install

Add the package to your Cabal project:

```cabal
build-depends:
  sendgrid-haskell
```

## Setup

Create a SendGrid API key with Mail Send permission, then provide it through your application config or environment.

```haskell
import qualified Data.Text as Text
import System.Environment (getEnv)

loadApiKey :: IO ApiKey
loadApiKey = ApiKey . Text.pack <$> getEnv "SENDGRID_API_KEY"
```

## Usage

Send a message with an API key and a typed v3 Mail Send payload:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Sendgrid.Api

main :: IO ()
main = do
  let client =
        defaultClient (ApiKey "SG.xxxxxx")
      message =
        simpleMail
          (namedEmailAddress "Example App" "sender@example.com")
          [emailAddress "person@example.com"]
          "Welcome"
          [ plainTextContent "Hello from Haskell.",
            htmlContent "<p>Hello from Haskell.</p>"
          ]

  result <- sendMail client message
  case result of
    Right response ->
      print (sendGridResponseMessageId response)
    Left err ->
      print err
```

## Templates and metadata

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value(String))
import qualified Data.Map.Strict as Map
import Network.Sendgrid.Api

message :: SendGridMail
message =
  (simpleMail
     (emailAddress "sender@example.com")
     [emailAddress "person@example.com"]
     "Welcome"
     [])
    { mailTemplateId = Just "d-1234567890",
      mailCategories = ["onboarding"],
      mailCustomArgs = Map.fromList [("tenant", "acme")],
      mailPersonalizations =
        [ (personalization [emailAddress "person@example.com"])
            { personalizationDynamicTemplateData =
                Map.fromList [("first_name", String "Ada")],
              personalizationCc = [emailAddress "manager@example.com"],
              personalizationBcc = [emailAddress "audit@example.com"]
            }
        ]
    }
```

## Attachments

`attachment` expects base64 encoded content, as required by SendGrid.

```haskell
messageWithAttachment :: SendGridMail
messageWithAttachment =
  (simpleMail
     (emailAddress "sender@example.com")
     [emailAddress "person@example.com"]
     "Report"
     [plainTextContent "The report is attached."])
    { mailAttachments =
        [ (attachment "SGVsbG8=" "report.txt")
            { attachmentType = Just "text/plain" }
        ]
    }
```

## Test

```sh
cabal build all
cabal test all
cabal check
```

## Notes

- `ApiKey` hides its value in `Show`.
- Non-202 responses return `SendGridUnexpectedStatus` with the raw response body and a parsed SendGrid error response when possible.
- `mailSendRequest` is exposed for tests, inspection, and applications that need custom HTTP execution.

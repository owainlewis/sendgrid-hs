 Sendgrid API
 -------------------------------

Haskell utility for sending Email with Sendgrid

You will need a Sendgrid username and password to use this library.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           Network.Sendgrid.Api

sendWelcomeMessage :: IO (Maybe MailSuccess)
sendWelcomeMessage = sendEmail (Authentication "" "") message
    where message = EmailMessage { to = "owain@owainlewis.com"
                                 , from = "noreply@vacancy.io"
                                 , subject = "Hello"
                                 , text = Just "Oh Hai there!"
                                 , html = Nothing }
```

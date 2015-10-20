 #Sendgrid
 
A Haskell library for sending email with Sendgrid

You will need a Sendgrid username and password to use this library.

```haskell
module Main where

import           Network.Sendgrid.Api

sendWelcomeMessage :: IO (Maybe MailSuccess)
sendWelcomeMessage = sendEmail (Authentication "USER" "PASSWORD") message
    where message = EmailMessage { to = "owain@owainlewis.com"
                                 , from = "noreply@vacancy.io"
                                 , subject = "Hello"
                                 , text = Just "Oh Hai there!"
                                 , html = Nothing }
```

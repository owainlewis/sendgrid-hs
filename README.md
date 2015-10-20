![](https://assets3.sendgrid.com/mkt/assets/logos_brands/horizontal/PNG/logo_full_color_flat-a982ded4b0b159db49a552f2cf490439.png)
 
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

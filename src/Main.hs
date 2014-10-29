module Main ( ) where

import Network.Sendgrid.Api
import Network.Sendgrid.Utils

message :: EmailMessage
message = EmailMessage { to = "owain@owainlewis.com"
                       , from = "noreply@vacancy.io"
                       , subject = "Hello"
                       , text = "Oh Hai there!" }

-- sendEmail (Authentication "" "")

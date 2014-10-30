{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Network.Sendgrid.Api

sendWelcomeMessage = sendEmail (Authentication "" "") message
    where message = EmailMessage { to = "owain@owainlewis.com"
                                 , from = "noreply@vacancy.io"
                                 , subject = "Hello"
                                 , text = "Oh Hai there!" }



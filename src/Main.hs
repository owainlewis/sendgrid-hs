{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
  ( sendWelcomeMessage
  , main )
  where

import           Network.Sendgrid.Api

sendWelcomeMessage :: IO (Maybe MailSuccess)
sendWelcomeMessage = sendEmail (Authentication "" "") message
    where message = EmailMessage { to = "owain@owainlewis.com"
                                 , from = "noreply@vacancy.io"
                                 , subject = "Hello"
                                 , text = Just "Oh Hai there!"
                                 , html = Nothing }

main :: IO (Maybe MailSuccess)
main = sendWelcomeMessage


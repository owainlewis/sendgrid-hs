{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Network.Sendgrid.Api
  ( Authentication(..)
  , EmailMessage(..)
  , MailSuccess(..)
  , makeRequest
  , getRequest
  , postRequest
  , sendEmail
  ) where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Monad               (mzero)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as L
import           Data.List                   (partition)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Network.HTTP.Conduit
import           Network.Sendgrid.Utils      (urlEncode)

-- | The base url for the Sendgrid API
--
baseUrl :: String
baseUrl = "https://api.sendgrid.com/api/"

class Tupled a where
    asTuple :: a -> [(String, String)]

-- | Auth

data Authentication = Authentication
  { user :: String
  , key  :: String
  } deriving ( Show )

instance Tupled Authentication where
  asTuple a =
    [ ("api_user", u)
    , ("api_key", k) ]
    where u = user a
          k = key a

-- | Messages

data EmailMessage = EmailMessage {
    to      :: String
  , from    :: String
  , subject :: String
  , text    :: Maybe String
  , html    :: Maybe String
} deriving ( Eq, Show )

instance Tupled EmailMessage where
    asTuple a =
      let t = (to a)
          f = (from a)
          s = (subject a)
          x = (text a)
          h = (html a) in
      [ ("to", t)
      , ("from", f)
      , ("subject", s)
      , ("text", fromMaybe "" x)
      , ("html", fromMaybe "" h) ]

-- | Helper function to encoding URLs

urlEncodeVars :: [(String,String)] -> String
urlEncodeVars [] = []
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff

data Method = GET | POST

class AsByteString a where
    asByteString :: a -> BS.ByteString

showBS :: Show a => a -> BS.ByteString
showBS = BS.pack . show

instance AsByteString Method where
  asByteString = showBS

instance Show Method where
    show GET  = "GET"
    show POST = "POST"

-- | HTTP request helpers
makeRequest :: (MonadBaseControl IO m,
                MonadIO m,
                MonadThrow m, Show a) =>
    a -> String -> [(String, String)] -> m L.ByteString
makeRequest method url body =
  let rBody = BS.pack . urlEncodeVars $ body in
  do
    initReq <- parseUrl url
    let req = initReq
              { method = showBS method
              , requestHeaders = [ ("content-type", "application/x-www-form-urlencoded") ]
              , requestBody = RequestBodyBS $ rBody
              }
    response <- withManager $ httpLbs req
    return $ responseBody response

-- | Request helpers

postRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
  String ->
  [ (String, String) ] ->
  m L.ByteString
postRequest = makeRequest POST

getRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
  String ->
  [ (String, String) ] ->
  m L.ByteString
getRequest = makeRequest GET

-- | Get user profile

data Profile = Profile {
    profileUsername  :: String
  , profileEmail     :: String
  , profileActive    :: String
  , profileFirstName :: String
  , profileLastName  :: String
  , profileAddress   :: String
  , profileCity      :: String
  , profileState     :: String
  , profileZip       :: String
  , profileCountry   :: String
  , profilePhone     :: String
  , profileWebsite   :: String
} deriving ( Show )

instance Aeson.FromJSON Profile where
    parseJSON (Aeson.Object o) =
        Profile <$>
          o Aeson..: "username"   <*>
          o Aeson..: "email"      <*>
          o Aeson..: "active"     <*>
          o Aeson..: "first_name" <*>
          o Aeson..: "last_name"  <*>
          o Aeson..: "address"    <*>
          o Aeson..: "city"       <*>
          o Aeson..: "state"      <*>
          o Aeson..: "zip"        <*>
          o Aeson..: "country"    <*>
          o Aeson..: "phone"      <*>
          o Aeson..: "website"
    parseJSON _ = mzero

getProfile :: (MonadThrow m, MonadIO m, MonadBaseControl IO m, Tupled a) =>
  a ->
  m (L.ByteString)
getProfile auth = 
    makeRequest POST (baseUrl <> "profile.get.json") (asTuple auth)

data MailSuccess = MailSuccess {
  message :: String
} deriving ( Show )

instance Aeson.FromJSON MailSuccess where
    parseJSON (Aeson.Object o) = MailSuccess <$> o Aeson..: "message"
    parseJSON _ = mzero

-- | Send an email message
--   i.e sendEmail (Authentication "FOO" "BAR") (Message ...)
sendEmail :: (Tupled a, Tupled b) =>
  a ->
  b ->
  IO (Maybe MailSuccess)
sendEmail auth message =
  let fullUrl = baseUrl <> "mail.send.json"
      response = makeRequest POST fullUrl (asTuple auth <> asTuple message) in
  Aeson.decode <$> response

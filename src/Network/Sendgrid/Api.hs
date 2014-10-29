{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sendgrid.Api
  ( Authentication(..)
  , EmailMessage(..)
  , postRequest
  , sendEmail
  ) where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Monoid((<>))
import Data.List(partition)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Control
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
-- import Control.Monad.Trans.Control.MonadBaseControl
import Network.Sendgrid.Utils (urlEncode)

-- | The base url for the Sendgrid API
--
baseUrl :: String
baseUrl = "https://api.sendgrid.com/api/"

-- | Form params will be represented as concrete Haskell types that get converted to simple tuples
--   when posting via conduit
class Tupled a where
    asTuple :: a -> [(String, String)]

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- | Messages
data EmailMessage = EmailMessage {
    to      :: String
  , from    :: String
  , subject :: String
  , text    :: String
} deriving ( Eq, Show )

instance Tupled EmailMessage where
    asTuple a =
      let t = (to a)
          f = (from a)
          s = (subject a)
          x = (text a) in
      [ ("to", t)
      , ("from", f)
      , ("subject", s)
      , ("text", x) ]

------------------------------------------------------------------------------
-- | Helper function to encoding URLs
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars [] = []
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff

------------------------------------------------------------------------------
-- | HTTP request helpers
postRequest :: (MonadThrow m,
                MonadIO m,
                Control.Monad.Trans.Control.MonadBaseControl IO m) =>
                String -> [(String, String)] -> m (Response L.ByteString)
postRequest url body = do
  initReq <- parseUrl url
  let req = initReq
            { method = "POST"
            , requestHeaders = [ ("content-type", "application/x-www-form-urlencoded") ]
            , requestBody = RequestBodyBS $ BS.pack . urlEncodeVars $ body
            }
  response <- withManager $ httpLbs req
  return response

------------------------------------------------------------------------------
-- | 
sendEmail
  :: (Tupled a1, Tupled a, MonadThrow m, MonadIO m, MonadBaseControl IO m) => 
      a -> a1 -> m (Response L.ByteString)
sendEmail auth message =
  let fullUrl = baseUrl <> "mail.send.json" in
  postRequest fullUrl (asTuple auth <> asTuple message)

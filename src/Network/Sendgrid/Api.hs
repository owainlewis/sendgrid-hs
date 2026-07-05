{-# LANGUAGE OverloadedStrings #-}

module Network.Sendgrid.Api
  ( ApiKey (..),
    SendGridClient (..),
    defaultClient,
    SendGridResponse (..),
    SendGridError (..),
    SendGridErrorResponse (..),
    SendGridErrorMessage (..),
    parseSendGridErrorResponse,
    EmailAddress (..),
    emailAddress,
    namedEmailAddress,
    ContentType (..),
    Content (..),
    plainTextContent,
    htmlContent,
    Personalization (..),
    personalization,
    AttachmentDisposition (..),
    Attachment (..),
    attachment,
    Asm (..),
    Toggle (..),
    BccSetting (..),
    FooterSetting (..),
    SpamCheckSetting (..),
    MailSettings (..),
    ClickTrackingSetting (..),
    OpenTrackingSetting (..),
    SubscriptionTrackingSetting (..),
    GanalyticsSetting (..),
    TrackingSettings (..),
    SendGridMail (..),
    simpleMail,
    validateMail,
    mailSendRequest,
    sendMail,
    sendMailWithManager,
  )
where

import qualified Control.Exception as Exception
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
    eitherDecode,
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Request (..),
    RequestBody (..),
    Response,
    httpLbs,
    newManager,
    parseRequest_,
    responseBody,
    responseHeaders,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, statusCode)

newtype ApiKey = ApiKey
  { unApiKey :: Text.Text
  }
  deriving (Eq, Ord)

instance Show ApiKey where
  show _ = "ApiKey <redacted>"

data SendGridClient = SendGridClient
  { sendGridApiKey :: ApiKey,
    sendGridBaseUrl :: String
  }
  deriving (Eq, Show)

defaultClient :: ApiKey -> SendGridClient
defaultClient apiKey =
  SendGridClient
    { sendGridApiKey = apiKey,
      sendGridBaseUrl = "https://api.sendgrid.com"
    }

data EmailAddress = EmailAddress
  { addressEmail :: Text.Text,
    addressName :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON EmailAddress where
  toJSON address =
    object $
      [ "email" .= addressEmail address
      ]
        ++ optional "name" (addressName address)

emailAddress :: Text.Text -> EmailAddress
emailAddress value =
  EmailAddress
    { addressEmail = value,
      addressName = Nothing
    }

namedEmailAddress :: Text.Text -> Text.Text -> EmailAddress
namedEmailAddress name value =
  EmailAddress
    { addressEmail = value,
      addressName = Just name
    }

data ContentType
  = ContentTextPlain
  | ContentTextHtml
  deriving (Eq, Show)

instance ToJSON ContentType where
  toJSON ContentTextPlain = "text/plain"
  toJSON ContentTextHtml = "text/html"

data Content = Content
  { contentType :: ContentType,
    contentValue :: Text.Text
  }
  deriving (Eq, Show)

instance ToJSON Content where
  toJSON content =
    object
      [ "type" .= contentType content,
        "value" .= contentValue content
      ]

plainTextContent :: Text.Text -> Content
plainTextContent value =
  Content
    { contentType = ContentTextPlain,
      contentValue = value
    }

htmlContent :: Text.Text -> Content
htmlContent value =
  Content
    { contentType = ContentTextHtml,
      contentValue = value
    }

data Personalization = Personalization
  { personalizationTo :: [EmailAddress],
    personalizationCc :: [EmailAddress],
    personalizationBcc :: [EmailAddress],
    personalizationSubject :: Maybe Text.Text,
    personalizationHeaders :: Map.Map Text.Text Text.Text,
    personalizationCustomArgs :: Map.Map Text.Text Text.Text,
    personalizationDynamicTemplateData :: Map.Map Text.Text Value,
    personalizationSendAt :: Maybe Int
  }
  deriving (Eq, Show)

instance ToJSON Personalization where
  toJSON value =
    object $
      [ "to" .= personalizationTo value
      ]
        ++ optionalList "cc" (personalizationCc value)
        ++ optionalList "bcc" (personalizationBcc value)
        ++ optional "subject" (personalizationSubject value)
        ++ optionalMap "headers" (personalizationHeaders value)
        ++ optionalMap "custom_args" (personalizationCustomArgs value)
        ++ optionalMap "dynamic_template_data" (personalizationDynamicTemplateData value)
        ++ optional "send_at" (personalizationSendAt value)

personalization :: [EmailAddress] -> Personalization
personalization recipients =
  Personalization
    { personalizationTo = recipients,
      personalizationCc = [],
      personalizationBcc = [],
      personalizationSubject = Nothing,
      personalizationHeaders = Map.empty,
      personalizationCustomArgs = Map.empty,
      personalizationDynamicTemplateData = Map.empty,
      personalizationSendAt = Nothing
    }

data AttachmentDisposition
  = DispositionAttachment
  | DispositionInline
  deriving (Eq, Show)

instance ToJSON AttachmentDisposition where
  toJSON DispositionAttachment = "attachment"
  toJSON DispositionInline = "inline"

data Attachment = Attachment
  { attachmentContent :: Text.Text,
    attachmentType :: Maybe Text.Text,
    attachmentFilename :: Text.Text,
    attachmentDisposition :: Maybe AttachmentDisposition,
    attachmentContentId :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON Attachment where
  toJSON value =
    object $
      [ "content" .= attachmentContent value,
        "filename" .= attachmentFilename value
      ]
        ++ optional "type" (attachmentType value)
        ++ optional "disposition" (attachmentDisposition value)
        ++ optional "content_id" (attachmentContentId value)

attachment :: Text.Text -> Text.Text -> Attachment
attachment base64Content filename =
  Attachment
    { attachmentContent = base64Content,
      attachmentType = Nothing,
      attachmentFilename = filename,
      attachmentDisposition = Just DispositionAttachment,
      attachmentContentId = Nothing
    }

data Asm = Asm
  { asmGroupId :: Int,
    asmGroupsToDisplay :: [Int]
  }
  deriving (Eq, Show)

instance ToJSON Asm where
  toJSON value =
    object $
      [ "group_id" .= asmGroupId value
      ]
        ++ optionalList "groups_to_display" (asmGroupsToDisplay value)

newtype Toggle = Toggle
  { toggleEnable :: Bool
  }
  deriving (Eq, Show)

instance ToJSON Toggle where
  toJSON value =
    object ["enable" .= toggleEnable value]

data BccSetting = BccSetting
  { bccSettingEnable :: Bool,
    bccSettingEmail :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON BccSetting where
  toJSON value =
    object $
      [ "enable" .= bccSettingEnable value
      ]
        ++ optional "email" (bccSettingEmail value)

data FooterSetting = FooterSetting
  { footerSettingEnable :: Bool,
    footerSettingText :: Maybe Text.Text,
    footerSettingHtml :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON FooterSetting where
  toJSON value =
    object $
      [ "enable" .= footerSettingEnable value
      ]
        ++ optional "text" (footerSettingText value)
        ++ optional "html" (footerSettingHtml value)

data SpamCheckSetting = SpamCheckSetting
  { spamCheckSettingEnable :: Bool,
    spamCheckSettingThreshold :: Maybe Int,
    spamCheckSettingPostToUrl :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON SpamCheckSetting where
  toJSON value =
    object $
      [ "enable" .= spamCheckSettingEnable value
      ]
        ++ optional "threshold" (spamCheckSettingThreshold value)
        ++ optional "post_to_url" (spamCheckSettingPostToUrl value)

data MailSettings = MailSettings
  { mailSettingsBcc :: Maybe BccSetting,
    mailSettingsBypassListManagement :: Maybe Toggle,
    mailSettingsFooter :: Maybe FooterSetting,
    mailSettingsSandboxMode :: Maybe Toggle,
    mailSettingsSpamCheck :: Maybe SpamCheckSetting
  }
  deriving (Eq, Show)

instance ToJSON MailSettings where
  toJSON value =
    object $
      optional "bcc" (mailSettingsBcc value)
        ++ optional "bypass_list_management" (mailSettingsBypassListManagement value)
        ++ optional "footer" (mailSettingsFooter value)
        ++ optional "sandbox_mode" (mailSettingsSandboxMode value)
        ++ optional "spam_check" (mailSettingsSpamCheck value)

data ClickTrackingSetting = ClickTrackingSetting
  { clickTrackingEnable :: Bool,
    clickTrackingEnableText :: Maybe Bool
  }
  deriving (Eq, Show)

instance ToJSON ClickTrackingSetting where
  toJSON value =
    object $
      [ "enable" .= clickTrackingEnable value
      ]
        ++ optional "enable_text" (clickTrackingEnableText value)

newtype OpenTrackingSetting = OpenTrackingSetting
  { openTrackingEnable :: Bool
  }
  deriving (Eq, Show)

instance ToJSON OpenTrackingSetting where
  toJSON value =
    object ["enable" .= openTrackingEnable value]

data SubscriptionTrackingSetting = SubscriptionTrackingSetting
  { subscriptionTrackingEnable :: Bool,
    subscriptionTrackingText :: Maybe Text.Text,
    subscriptionTrackingHtml :: Maybe Text.Text,
    subscriptionTrackingSubstitutionTag :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON SubscriptionTrackingSetting where
  toJSON value =
    object $
      [ "enable" .= subscriptionTrackingEnable value
      ]
        ++ optional "text" (subscriptionTrackingText value)
        ++ optional "html" (subscriptionTrackingHtml value)
        ++ optional "substitution_tag" (subscriptionTrackingSubstitutionTag value)

data GanalyticsSetting = GanalyticsSetting
  { ganalyticsEnable :: Bool,
    ganalyticsUtmSource :: Maybe Text.Text,
    ganalyticsUtmMedium :: Maybe Text.Text,
    ganalyticsUtmTerm :: Maybe Text.Text,
    ganalyticsUtmContent :: Maybe Text.Text,
    ganalyticsUtmCampaign :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance ToJSON GanalyticsSetting where
  toJSON value =
    object $
      [ "enable" .= ganalyticsEnable value
      ]
        ++ optional "utm_source" (ganalyticsUtmSource value)
        ++ optional "utm_medium" (ganalyticsUtmMedium value)
        ++ optional "utm_term" (ganalyticsUtmTerm value)
        ++ optional "utm_content" (ganalyticsUtmContent value)
        ++ optional "utm_campaign" (ganalyticsUtmCampaign value)

data TrackingSettings = TrackingSettings
  { trackingClick :: Maybe ClickTrackingSetting,
    trackingOpen :: Maybe OpenTrackingSetting,
    trackingSubscription :: Maybe SubscriptionTrackingSetting,
    trackingGanalytics :: Maybe GanalyticsSetting
  }
  deriving (Eq, Show)

instance ToJSON TrackingSettings where
  toJSON value =
    object $
      optional "click_tracking" (trackingClick value)
        ++ optional "open_tracking" (trackingOpen value)
        ++ optional "subscription_tracking" (trackingSubscription value)
        ++ optional "ganalytics" (trackingGanalytics value)

data SendGridMail = SendGridMail
  { mailPersonalizations :: [Personalization],
    mailFrom :: EmailAddress,
    mailReplyTo :: Maybe EmailAddress,
    mailSubject :: Maybe Text.Text,
    mailContent :: [Content],
    mailAttachments :: [Attachment],
    mailTemplateId :: Maybe Text.Text,
    mailCategories :: [Text.Text],
    mailCustomArgs :: Map.Map Text.Text Text.Text,
    mailSendAt :: Maybe Int,
    mailBatchId :: Maybe Text.Text,
    mailAsm :: Maybe Asm,
    mailMailSettings :: Maybe MailSettings,
    mailTrackingSettings :: Maybe TrackingSettings
  }
  deriving (Eq, Show)

instance ToJSON SendGridMail where
  toJSON value =
    object $
      [ "personalizations" .= mailPersonalizations value,
        "from" .= mailFrom value
      ]
        ++ optional "reply_to" (mailReplyTo value)
        ++ optional "subject" (mailSubject value)
        ++ optionalList "content" (mailContent value)
        ++ optionalList "attachments" (mailAttachments value)
        ++ optional "template_id" (mailTemplateId value)
        ++ optionalList "categories" (mailCategories value)
        ++ optionalMap "custom_args" (mailCustomArgs value)
        ++ optional "send_at" (mailSendAt value)
        ++ optional "batch_id" (mailBatchId value)
        ++ optional "asm" (mailAsm value)
        ++ optional "mail_settings" (mailMailSettings value)
        ++ optional "tracking_settings" (mailTrackingSettings value)

simpleMail :: EmailAddress -> [EmailAddress] -> Text.Text -> [Content] -> SendGridMail
simpleMail sender recipients subject contents =
  SendGridMail
    { mailPersonalizations = [personalization recipients],
      mailFrom = sender,
      mailReplyTo = Nothing,
      mailSubject = Just subject,
      mailContent = contents,
      mailAttachments = [],
      mailTemplateId = Nothing,
      mailCategories = [],
      mailCustomArgs = Map.empty,
      mailSendAt = Nothing,
      mailBatchId = Nothing,
      mailAsm = Nothing,
      mailMailSettings = Nothing,
      mailTrackingSettings = Nothing
    }

data SendGridResponse = SendGridResponse
  { sendGridResponseStatus :: Status,
    sendGridResponseHeaders :: ResponseHeaders,
    sendGridResponseMessageId :: Maybe Text.Text
  }
  deriving (Eq, Show)

data SendGridError
  = SendGridValidationError [Text.Text]
  | SendGridHttpException HttpException
  | SendGridUnexpectedStatus Status LazyByteString.ByteString (Maybe SendGridErrorResponse)
  deriving (Show)

data SendGridErrorResponse = SendGridErrorResponse
  { sendGridErrors :: [SendGridErrorMessage]
  }
  deriving (Eq, Show)

instance FromJSON SendGridErrorResponse where
  parseJSON =
    withObject "SendGridErrorResponse" $ \value ->
      SendGridErrorResponse
        <$> value .: "errors"

data SendGridErrorMessage = SendGridErrorMessage
  { sendGridErrorMessage :: Text.Text,
    sendGridErrorField :: Maybe Text.Text,
    sendGridErrorHelp :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance FromJSON SendGridErrorMessage where
  parseJSON =
    withObject "SendGridErrorMessage" $ \value ->
      SendGridErrorMessage
        <$> value .: "message"
        <*> value .:? "field"
        <*> value .:? "help"

parseSendGridErrorResponse :: LazyByteString.ByteString -> Either String SendGridErrorResponse
parseSendGridErrorResponse =
  eitherDecode

mailSendRequest :: SendGridClient -> SendGridMail -> Request
mailSendRequest client message =
  request
    { method = "POST",
      requestHeaders =
        [ ("Authorization", bearerAuthorization (sendGridApiKey client)),
          ("Content-Type", "application/json"),
          ("Accept", "application/json")
        ],
      requestBody = RequestBodyLBS (encode message)
    }
  where
    request = parseRequest_ (sendGridBaseUrl client <> "/v3/mail/send")

sendMail :: SendGridClient -> SendGridMail -> IO (Either SendGridError SendGridResponse)
sendMail client message = do
  manager <- newManager tlsManagerSettings
  sendMailWithManager manager client message

sendMailWithManager ::
  Manager ->
  SendGridClient ->
  SendGridMail ->
  IO (Either SendGridError SendGridResponse)
sendMailWithManager manager client message =
  case validateMail message of
    Left errors -> pure (Left (SendGridValidationError errors))
    Right () -> do
      result <- Exception.try (httpLbs (mailSendRequest client message) manager)
      pure $ case result of
        Left err -> Left (SendGridHttpException err)
        Right response -> responseToResult response

validateMail :: SendGridMail -> Either [Text.Text] ()
validateMail message =
  case validationErrors message of
    [] -> Right ()
    errors -> Left errors

validationErrors :: SendGridMail -> [Text.Text]
validationErrors message =
  concat
    [ ["at least one personalization is required" | null (mailPersonalizations message)],
      [ "each personalization must include at least one to recipient"
        | any (null . personalizationTo) (mailPersonalizations message)
      ],
      [ "content is required when template_id is not set"
        | null (mailContent message) && mailTemplateId message == Nothing
      ]
    ]

responseToResult ::
  Response LazyByteString.ByteString ->
  Either SendGridError SendGridResponse
responseToResult response
  | statusCode status == 202 =
      Right
        SendGridResponse
          { sendGridResponseStatus = status,
            sendGridResponseHeaders = headers,
            sendGridResponseMessageId = decodeHeaderText <$> lookup (mk "X-Message-Id") headers
          }
  | otherwise =
      Left (SendGridUnexpectedStatus status body parsedBody)
  where
    status = responseStatus response
    headers = responseHeaders response
    body = responseBody response
    parsedBody = either (const Nothing) Just (parseSendGridErrorResponse body)

bearerAuthorization :: ApiKey -> ByteString.ByteString
bearerAuthorization (ApiKey value) =
  "Bearer " <> Text.encodeUtf8 value

decodeHeaderText :: ByteString.ByteString -> Text.Text
decodeHeaderText =
  Text.decodeUtf8

optional :: ToJSON value => Text.Text -> Maybe value -> [Pair]
optional name =
  maybe [] (\value -> [fromText name .= value])

optionalList :: ToJSON value => Text.Text -> [value] -> [Pair]
optionalList name values
  | null values = []
  | otherwise = [fromText name .= values]

optionalMap :: ToJSON value => Text.Text -> Map.Map Text.Text value -> [Pair]
optionalMap name values
  | Map.null values = []
  | otherwise = [fromText name .= values]

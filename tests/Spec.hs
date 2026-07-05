{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value (String), decode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Network.HTTP.Client (RequestBody (..), host, method, path, requestBody, requestHeaders, secure)
import Network.Sendgrid.Api
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "SendGridMail JSON" $ do
      it "encodes a v3 Mail Send request with supported message fields" $ do
        toJSON richMessage
          `shouldBe` object
            [ "personalizations"
                .= [ object
                       [ "to" .= [object ["email" .= ("to@example.com" :: Text)]],
                         "cc" .= [object ["email" .= ("cc@example.com" :: Text)]],
                         "bcc" .= [object ["email" .= ("bcc@example.com" :: Text)]],
                         "subject" .= ("Personal subject" :: Text),
                         "headers" .= object ["X-Campaign" .= ("welcome" :: Text)],
                         "custom_args" .= object ["customer_id" .= ("42" :: Text)],
                         "dynamic_template_data" .= object ["name" .= ("Ada" :: Text)],
                         "send_at" .= (1710000000 :: Int)
                       ]
                   ],
              "from" .= object ["email" .= ("sender@example.com" :: Text), "name" .= ("Sender" :: Text)],
              "reply_to" .= object ["email" .= ("reply@example.com" :: Text)],
              "subject" .= ("Welcome" :: Text),
              "content"
                .= [ object ["type" .= ("text/plain" :: Text), "value" .= ("Hello" :: Text)],
                     object ["type" .= ("text/html" :: Text), "value" .= ("<p>Hello</p>" :: Text)]
                   ],
              "attachments"
                .= [ object
                       [ "content" .= ("SGVsbG8=" :: Text),
                         "filename" .= ("hello.txt" :: Text),
                         "type" .= ("text/plain" :: Text),
                         "disposition" .= ("attachment" :: Text),
                         "content_id" .= ("hello" :: Text)
                       ]
                   ],
              "template_id" .= ("d-template" :: Text),
              "categories" .= [("onboarding" :: Text)],
              "custom_args" .= object ["tenant" .= ("acme" :: Text)],
              "send_at" .= (1710000001 :: Int),
              "batch_id" .= ("batch-1" :: Text),
              "asm" .= object ["group_id" .= (7 :: Int), "groups_to_display" .= [7 :: Int, 8]],
              "mail_settings"
                .= object
                  [ "bcc" .= object ["enable" .= True, "email" .= ("archive@example.com" :: Text)],
                    "bypass_list_management" .= object ["enable" .= False],
                    "footer" .= object ["enable" .= True, "text" .= ("footer" :: Text)],
                    "sandbox_mode" .= object ["enable" .= True],
                    "spam_check" .= object ["enable" .= True, "threshold" .= (5 :: Int)]
                  ],
              "tracking_settings"
                .= object
                  [ "click_tracking" .= object ["enable" .= True, "enable_text" .= True],
                    "open_tracking" .= object ["enable" .= True],
                    "subscription_tracking"
                      .= object
                        [ "enable" .= True,
                          "text" .= ("unsubscribe" :: Text),
                          "substitution_tag" .= ("[%unsubscribe%]" :: Text)
                        ],
                    "ganalytics"
                      .= object
                        [ "enable" .= True,
                          "utm_source" .= ("newsletter" :: Text),
                          "utm_campaign" .= ("welcome" :: Text)
                        ]
                  ]
            ]

    describe "mailSendRequest" $ do
      it "builds a POST request to /v3/mail/send with Bearer auth and JSON body" $ do
        let request = mailSendRequest (defaultClient (ApiKey "SG.test")) richMessage

        method request `shouldBe` "POST"
        secure request `shouldBe` True
        host request `shouldBe` "api.sendgrid.com"
        path request `shouldBe` "/v3/mail/send"
        lookup "Authorization" (requestHeaders request) `shouldBe` Just "Bearer SG.test"
        lookup "Content-Type" (requestHeaders request) `shouldBe` Just "application/json"
        lookup "Accept" (requestHeaders request) `shouldBe` Just "application/json"
        decodeRequestBody (requestBody request) `shouldBe` Just (toJSON richMessage)

    describe "parseSendGridErrorResponse" $ do
      it "parses structured SendGrid error responses" $ do
        parseSendGridErrorResponse
          "{\"errors\":[{\"message\":\"The from address does not match a verified Sender Identity.\",\"field\":\"from.email\",\"help\":\"https://sendgrid.com/docs/for-developers/sending-email/sender-identity/\"}]}"
          `shouldBe` Right
            ( SendGridErrorResponse
                [ SendGridErrorMessage
                    "The from address does not match a verified Sender Identity."
                    (Just "from.email")
                    (Just "https://sendgrid.com/docs/for-developers/sending-email/sender-identity/")
                ]
            )

    describe "validateMail" $ do
      it "reports invalid requests before HTTP execution" $ do
        validateMail
          ( SendGridMail
              { mailPersonalizations = [],
                mailFrom = emailAddress "sender@example.com",
                mailReplyTo = Nothing,
                mailSubject = Just "Hello",
                mailContent = [],
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
          )
          `shouldBe` Left
            [ "at least one personalization is required",
              "content is required when template_id is not set"
            ]

      it "requires a subject source for non-template requests" $ do
        validateMail
          ( (simpleMail
                (emailAddress "sender@example.com")
                [emailAddress "person@example.com"]
                "Hello"
                [plainTextContent "Body"]
            )
              { mailSubject = Nothing
              }
          )
          `shouldBe` Left
            [ "subject is required when template_id is not set and any personalization has no subject"
            ]

richMessage :: SendGridMail
richMessage =
  SendGridMail
    { mailPersonalizations =
        [ (personalization [emailAddress "to@example.com"])
            { personalizationCc = [emailAddress "cc@example.com"],
              personalizationBcc = [emailAddress "bcc@example.com"],
              personalizationSubject = Just "Personal subject",
              personalizationHeaders = Map.fromList [("X-Campaign", "welcome")],
              personalizationCustomArgs = Map.fromList [("customer_id", "42")],
              personalizationDynamicTemplateData = Map.fromList [("name", String "Ada")],
              personalizationSendAt = Just 1710000000
            }
        ],
      mailFrom = namedEmailAddress "Sender" "sender@example.com",
      mailReplyTo = Just (emailAddress "reply@example.com"),
      mailSubject = Just "Welcome",
      mailContent = [plainTextContent "Hello", htmlContent "<p>Hello</p>"],
      mailAttachments =
        [ (attachment "SGVsbG8=" "hello.txt")
            { attachmentType = Just "text/plain",
              attachmentContentId = Just "hello"
            }
        ],
      mailTemplateId = Just "d-template",
      mailCategories = ["onboarding"],
      mailCustomArgs = Map.fromList [("tenant", "acme")],
      mailSendAt = Just 1710000001,
      mailBatchId = Just "batch-1",
      mailAsm = Just (Asm 7 [7, 8]),
      mailMailSettings =
        Just
          MailSettings
            { mailSettingsBcc = Just (BccSetting True (Just "archive@example.com")),
              mailSettingsBypassListManagement = Just (Toggle False),
              mailSettingsFooter = Just (FooterSetting True (Just "footer") Nothing),
              mailSettingsSandboxMode = Just (Toggle True),
              mailSettingsSpamCheck = Just (SpamCheckSetting True (Just 5) Nothing)
            },
      mailTrackingSettings =
        Just
          TrackingSettings
            { trackingClick = Just (ClickTrackingSetting True (Just True)),
              trackingOpen = Just (OpenTrackingSetting True),
              trackingSubscription =
                Just
                  SubscriptionTrackingSetting
                    { subscriptionTrackingEnable = True,
                      subscriptionTrackingText = Just "unsubscribe",
                      subscriptionTrackingHtml = Nothing,
                      subscriptionTrackingSubstitutionTag = Just "[%unsubscribe%]"
                    },
              trackingGanalytics =
                Just
                  GanalyticsSetting
                    { ganalyticsEnable = True,
                      ganalyticsUtmSource = Just "newsletter",
                      ganalyticsUtmMedium = Nothing,
                      ganalyticsUtmTerm = Nothing,
                      ganalyticsUtmContent = Nothing,
                      ganalyticsUtmCampaign = Just "welcome"
                    }
            }
    }

decodeRequestBody :: RequestBody -> Maybe Value
decodeRequestBody (RequestBodyLBS body) =
  decode body
decodeRequestBody (RequestBodyBS body) =
  decode (LazyByteString.fromStrict body)
decodeRequestBody _ =
  Nothing

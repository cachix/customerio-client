module CustomerIO.Track.Customers.Types.AddOrUpdateCustomerDevice
  ( AddOrUpdateCustomerDeviceBody(..)
  , DeviceAttributes(..)
  ) where

import CustomerIO.Aeson (leftoverObject, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (FromJSON(..), Object, ToJSON(toJSON), Value(..), object, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text, unpack)

data AddOrUpdateCustomerDeviceBody = MkAddOrUpdateCustomerDeviceBody
  { aucdDeviceId :: Text
  , aucdPlatform :: Text
  , aucdLastUsed :: Maybe Timestamp
  , aucdAttributes :: Maybe DeviceAttributes
  }
  deriving stock (Eq, Show)

instance ToJSON AddOrUpdateCustomerDeviceBody where
  toJSON MkAddOrUpdateCustomerDeviceBody {..} =
    object [ mkPair "device" device]
    where
      device = mkObject
        [ mkPair "id" <$> Just aucdDeviceId
        , mkPair "platform" <$> Just aucdPlatform
        , mkPair "last_used" <$> aucdLastUsed
        , mkPair "attributes" <$> aucdAttributes
        ]

instance FromJSON AddOrUpdateCustomerDeviceBody where
  parseJSON = withObject "AddOrUpdateCustomerDeviceBody" $ \o -> do
    device <- o .: "device"
    aucdDeviceId <- device .: "id"
    aucdPlatform <- device .: "platform"
    aucdLastUsed <- device .:? "last_used"
    aucdAttributes <- device .:? "attributes"
    pure MkAddOrUpdateCustomerDeviceBody {..}

data DeviceAttributes = MkDeviceAttributes
  { daDeviceOs :: Maybe Text
  , daDeviceModel :: Maybe Text
  , daAppVersion :: Maybe Text
  , daCioSdkVersion :: Maybe Text
  , daDeviceLocale :: Maybe Text
  , daPushEnabled :: Maybe Bool
  , daExtraDeviceAttributes :: Maybe Object
  }
  deriving stock (Eq, Show)

instance FromJSON DeviceAttributes where
    parseJSON = withObject "DeviceAttributes" $ \o -> do
      daDeviceOs <- o .:? "device_os"
      daDeviceModel <- o .:? "device_model"
      daAppVersion <- o .:? "app_version"
      daCioSdkVersion <- o .:? "cio_sdk_version"
      daDeviceLocale <- o .:? "device_locale"
      daPushEnabled <- do
        pushEnabledText <- o .:? "push_enabled"
        traverse textToBool pushEnabledText
      daExtraDeviceAttributes <- o `leftoverObject` knownKeys
      pure MkDeviceAttributes {..}
      where
        knownKeys = ["device_os", "device_model", "app_version", "cio_sdk_version", "device_locale", "push_enabled"]

instance ToJSON DeviceAttributes where
  toJSON MkDeviceAttributes {..} = case daExtraDeviceAttributes of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "device_os" <$> daDeviceOs
        , mkPair "device_model" <$> daDeviceModel
        , mkPair "app_version" <$> daAppVersion
        , mkPair "cio_sdk_version" <$> daCioSdkVersion
        , mkPair "device_locale" <$> daDeviceLocale
        , mkPair "push_enabled" . boolToText <$> daPushEnabled
        ]

boolToText :: Bool -> Text
boolToText False = "false"
boolToText True = "true"

textToBool :: Text -> Parser Bool
textToBool "false" = pure False
textToBool "true" = pure True
textToBool v = fail ("Unexpected value for push_enabled: " <> unpack v)

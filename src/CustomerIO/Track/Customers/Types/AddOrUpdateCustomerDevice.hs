module CustomerIO.Track.Customers.Types.AddOrUpdateCustomerDevice
  ( AddOrUpdateCustomerDeviceBody(..)
  , DeviceAttributes(..)
  ) where

import CustomerIO.Aeson (mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (Object, ToJSON(toJSON), Value(..), object)
import Data.Text (Text)

data AddOrUpdateCustomerDeviceBody = MkAddOrUpdateCustomerDeviceBody
  { aucdDeviceId :: Text
  , aucdPlatform :: Text
  , aucdLastUsed :: Maybe Timestamp
  , aucdAttributes :: Maybe DeviceAttributes
  }

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

data DeviceAttributes = MkDeviceAttributes
  { daDeviceOs :: Maybe Text
  , daDeviceModel :: Maybe Text
  , daAppVersion :: Maybe Text
  , daCioSdkVersion :: Maybe Text
  , daDeviceLocale :: Maybe Text
  , daPushEnabled :: Maybe Bool
  , daExtraDeviceAttributes :: Maybe Object
  }

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
boolToText True = "true"
boolToText False = "false"

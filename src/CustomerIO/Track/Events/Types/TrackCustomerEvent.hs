-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.Types.TrackCustomerEvent
  ( module CustomerIO.Track.Events.Types.TrackCustomerEvent
  ) where

import CustomerIO.Aeson (defaultAesonOptions, leftoverObject, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (FromJSON(..), Object, ToJSON(toJSON), Value(..), withObject, (.:?))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Text (Text)

data TrackCustomerEventBody = MkTrackCustomerEvent
  { tceName      :: Text
  , tceId        :: Maybe Text
  , tceTimestamp :: Maybe Timestamp
  , tceData      :: Maybe CustomerEventData
  }
  deriving stock (Eq, Show)

data CustomerEventData = MkCustomerEventData
  { cedRecipient        :: Maybe Text
  , cedFromAddress      :: Maybe Text
  , cedReplyTo          :: Maybe Text
  , cedAdditionalFields :: Maybe Object
  }
  deriving stock (Eq, Show)

instance FromJSON CustomerEventData where
  parseJSON = withObject "CustomerEventData" $ \o -> do
    cedRecipient <- o .:? "recipient"
    cedFromAddress <- o .:? "from_address"
    cedReplyTo <- o .:? "reply_to"
    cedAdditionalFields <- o `leftoverObject` knownFields
    pure MkCustomerEventData{..}
    where
      knownFields = ["recipient", "from_address", "reply_to"]

instance ToJSON CustomerEventData where
  toJSON MkCustomerEventData{..} = case cedAdditionalFields of
    Just af -> Object $ mainFields <> af
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "recipient" <$> cedRecipient
        , mkPair "from_address" <$> cedFromAddress
        , mkPair "reply_to" <$> cedReplyTo
        ]

defaultTrackCustomerEvent
  :: Text -- ^ name
  -> TrackCustomerEventBody
defaultTrackCustomerEvent name = MkTrackCustomerEvent name Nothing Nothing Nothing

defaultCustomerEventData :: Maybe Object -> CustomerEventData
defaultCustomerEventData = MkCustomerEventData Nothing Nothing Nothing

deriveFromJSON defaultAesonOptions ''TrackCustomerEventBody
deriveToJSON defaultAesonOptions ''TrackCustomerEventBody

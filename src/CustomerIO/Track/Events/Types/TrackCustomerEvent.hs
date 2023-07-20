-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.Types.TrackCustomerEvent
  ( module CustomerIO.Track.Events.Types.TrackCustomerEvent
  ) where

import CustomerIO.Aeson (defaultAesonOptions, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (FromJSON(..), Object, ToJSON(toJSON), Value(..), withObject, (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Text (Text)

data TrackCustomerEventBody = MkTrackCustomerEvent
  { tceName      :: Text
  , tceId        :: Maybe Text
  , tceTimestamp :: Maybe Timestamp
  , tceData      :: Maybe CustomerEventData
  }

data CustomerEventData = MkCustomerEventData
  { cedRecipient        :: Maybe Text
  , cedFromAddress      :: Maybe Text
  , cedReplyTo          :: Maybe Text
  , cedAdditionalFields :: Maybe Object
  }

instance FromJSON CustomerEventData where
  parseJSON = withObject "CustomerEventData" $ \o -> do
    cedRecipient <- o .:? "recipient"
    cedFromAddress <- o .:? "from_address"
    cedReplyTo <- o .:? "reply_to"
    let cedAdditionalFields = collectRest o
    pure MkCustomerEventData{..}
    where
      knownFields = ["recipient", "from_address", "reply_to"]
      collectRest obj =
        let remaining = KM.filterWithKey (\k _ -> k `notElem` knownFields) obj
        in if KM.null remaining
          then Nothing
          else Just remaining

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

deriveFromJSON defaultAesonOptions ''TrackCustomerEventBody
deriveToJSON defaultAesonOptions ''TrackCustomerEventBody

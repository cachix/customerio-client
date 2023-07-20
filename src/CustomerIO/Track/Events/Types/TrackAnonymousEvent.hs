-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.Types.TrackAnonymousEvent
  ( module CustomerIO.Track.Events.Types.TrackAnonymousEvent
  ) where

import CustomerIO.Aeson (defaultAesonOptions, leftoverObject, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Text (Text)

data TrackAnonymousEventBody
  = StandardAnonymousEventBody StandardAnonymousEvent
  | InviteAnonymousEventBody InviteAnonymousEvent
  deriving stock (Eq, Show)

data StandardAnonymousEvent = MkStandardAnonymousEvent
  { saeName        :: Text
  , saeAnonymousId :: Maybe Text
  , saeId          :: Maybe Text
  , saeTimestamp   :: Maybe Timestamp
  , saeData        :: Maybe StandardAnonymousData
  }
  deriving stock (Eq, Show)

data InviteAnonymousEvent = MkInviteAnonymousEvent
  { iaeName      :: Text
  , iaeData      :: InviteAnonymousData
  , iaeTimestamp :: Maybe Timestamp
  }
  deriving stock (Eq, Show)

data StandardAnonymousData = MkStandardAnonymousData
  { sadFromAddress      :: Maybe Text
  , sadReplyTo          :: Maybe Text
  , sadAdditionalFields :: Maybe Object
  }
  deriving stock (Eq, Show)

instance FromJSON StandardAnonymousData where
  parseJSON = withObject "StandardAnonymousData" $ \o -> do
    sadFromAddress <- o .:? "from_address"
    sadReplyTo <- o .:? "reply_to"
    sadAdditionalFields <- o `leftoverObject` knownFields
    pure MkStandardAnonymousData{..}
    where
      knownFields = ["from_address", "reply_to"]

instance ToJSON StandardAnonymousData where
  toJSON MkStandardAnonymousData{..} = case sadAdditionalFields of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "from_address" <$> sadFromAddress
        , mkPair "reply_to" <$> sadReplyTo
        ]

data InviteAnonymousData = MkInviteAnonymousData
  { iadRecipient        :: Text
  , iadFromAddress      :: Maybe Text
  , iadReplyTo          :: Maybe Text
  , iadAdditionalFields :: Maybe Object
  }
  deriving stock (Eq, Show)

instance FromJSON InviteAnonymousData where
  parseJSON = withObject "InviteAnonymousData" $ \o -> do
    iadRecipient <- o .: "recipient"
    iadFromAddress <- o .:? "from_address"
    iadReplyTo <- o .:? "reply_to"
    iadAdditionalFields <- o `leftoverObject` knownFields
    pure MkInviteAnonymousData{..}
    where
      knownFields = ["recipient", "from_address", "reply_to"]

instance ToJSON InviteAnonymousData where
  toJSON MkInviteAnonymousData{..} = case iadAdditionalFields of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ Just (mkPair "recipient" iadRecipient)
        , mkPair "from_address" <$> iadFromAddress
        , mkPair "reply_to" <$> iadReplyTo
        ]

defaultStandardAnonymousEvent
  :: Text -- ^ name
  -> StandardAnonymousEvent
defaultStandardAnonymousEvent name =
  MkStandardAnonymousEvent name Nothing  Nothing Nothing Nothing

defaultInviteAnonymousEvent
  :: Text -- ^ name
  -> Text -- ^ recipient (from `data` field)
  -> InviteAnonymousEvent
defaultInviteAnonymousEvent name recipient =
  MkInviteAnonymousEvent name (MkInviteAnonymousData recipient Nothing Nothing Nothing) Nothing

deriveFromJSON defaultAesonOptions ''StandardAnonymousEvent
deriveToJSON defaultAesonOptions ''StandardAnonymousEvent
deriveFromJSON defaultAesonOptions ''InviteAnonymousEvent
deriveToJSON defaultAesonOptions ''InviteAnonymousEvent
deriveFromJSON (defaultAesonOptions {sumEncoding = UntaggedValue}) ''TrackAnonymousEventBody
deriveToJSON (defaultAesonOptions {sumEncoding = UntaggedValue}) ''TrackAnonymousEventBody

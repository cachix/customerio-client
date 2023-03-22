-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.Types.TrackAnonymousEvent
  ( module CustomerIO.Track.Events.Types.TrackAnonymousEvent
  ) where

import CustomerIO.Aeson (defaultAesonOptions, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)

data TrackAnonymousEventBody
  = StandardAnonymousEventBody StandardAnonymousEvent
  | InviteAnonymousEventBody InviteAnonymousEvent

data StandardAnonymousEvent = MkStandardAnonymousEvent
  { saeName        :: Text
  , saeAnonymousId :: Text
  , saeId          :: Maybe Text
  , saeTimestamp   :: Maybe Timestamp
  , saeData        :: Maybe StandardAnonymousData
  }

data InviteAnonymousEvent = MkInviteAnonymousEvent
  { iaeName      :: Text
  , iaeData      :: InviteAnonymousData
  , iaeTimestamp :: Maybe Timestamp
  }

data StandardAnonymousData = MkStandardAnonymousData
  { sadFromAddress      :: Maybe Text
  , sadReplyTo          :: Maybe Text
  , sadAdditionalFields :: Maybe Object
  }

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
  -> Text -- ^ anonymous_id
  -> StandardAnonymousEvent
defaultStandardAnonymousEvent name anonymousId =
  MkStandardAnonymousEvent name anonymousId  Nothing Nothing Nothing

defaultInviteAnonymousEvent
  :: Text -- ^ name
  -> Text -- ^ recipient (from `data` field)
  -> InviteAnonymousEvent
defaultInviteAnonymousEvent name recipient =
  MkInviteAnonymousEvent name (MkInviteAnonymousData recipient Nothing Nothing Nothing) Nothing

deriveToJSON defaultAesonOptions ''StandardAnonymousEvent
deriveToJSON defaultAesonOptions ''InviteAnonymousEvent
deriveToJSON (defaultAesonOptions {sumEncoding = UntaggedValue}) ''TrackAnonymousEventBody

module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
  ( module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
  ) where

import CustomerIO.Aeson (defaultAesonOptions, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (FromJSON(..), Object, ToJSON(toJSON), Value(..), withObject, withText, (.:?))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Text (Text)

data AddOrUpdateCustomerBody = MkAddOrUpdateCustomerBody
  { aucId                         :: Maybe Text
  , aucEmail                      :: Maybe Text
  , aucAnonymousId                :: Maybe Text
  , aucCreatedAt                  :: Maybe Timestamp
  , auc_Update                    :: Maybe Bool
  , aucCioRelationships           :: Maybe CioRelationships
  , aucUnsubscribed               :: Maybe Bool
  , aucCioSubscriptionPreferences :: Maybe Object -- TODO
  , aucAttributes                 :: Maybe Object
  }

data CioRelationshipAction = AddCioRelationships | RemoveCioRelationships

data Identifier = MkIdentifier
    { iObjectTypeId :: Text
    , iObjectId :: Text
    }

data CioRelationships = MkCioRelationships
  { crAction :: CioRelationshipAction
  , crRelationships :: [Identifier]
  }

deriveFromJSON defaultAesonOptions ''Identifier
deriveToJSON defaultAesonOptions ''Identifier

instance FromJSON CioRelationshipAction where
  parseJSON = withText "CioRelationshipAction" $ \case
    "add_relationships" -> pure AddCioRelationships
    "remove_relationships" -> pure RemoveCioRelationships
    _ -> fail "Unknown CioRelationshipAction"

instance ToJSON CioRelationshipAction where
  toJSON AddCioRelationships = String "add_relationships"
  toJSON RemoveCioRelationships = String "remove_relationships"

deriveFromJSON defaultAesonOptions ''CioRelationships
deriveToJSON defaultAesonOptions ''CioRelationships

instance FromJSON AddOrUpdateCustomerBody where
  parseJSON = withObject "AddOrUpdateCustomerBody" $ \o -> do
    aucId <- o .:? "id"
    aucEmail <- o .:? "email"
    aucAnonymousId <- o .:? "anonymous_id"
    aucCreatedAt <- o .:? "created_at"
    auc_Update <- o .:? "_update"
    aucCioRelationships <- o .:? "cio_relationships"
    aucUnsubscribed <- o .:? "unsubscribed"
    aucCioSubscriptionPreferences <- o .:? "cio_subscription_preferences"
    aucAttributes <- o .:? "attributes"
    pure MkAddOrUpdateCustomerBody {..}

instance ToJSON AddOrUpdateCustomerBody where
  toJSON MkAddOrUpdateCustomerBody {..} = case aucAttributes of
    Just km -> Object (mainFields <> km)
    Nothing -> Object mainFields
    where
      mainFields = mkObject
        [ mkPair "id" <$> aucId
        , mkPair "email" <$> aucEmail
        , mkPair "anonymous_id" <$> aucAnonymousId
        , mkPair "created_at" <$> aucCreatedAt
        , mkPair "_update" <$> auc_Update
        , mkPair "cio_relationships" <$> aucCioRelationships
        , mkPair "unsubscribed" <$> aucUnsubscribed
        , mkPair "cio_subscription_preferences" <$> aucCioSubscriptionPreferences
        ]

defaultAddOrUpdateCustomerBody :: AddOrUpdateCustomerBody
defaultAddOrUpdateCustomerBody = MkAddOrUpdateCustomerBody Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

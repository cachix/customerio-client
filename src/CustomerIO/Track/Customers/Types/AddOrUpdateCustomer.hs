module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
  ( module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
  ) where

import CustomerIO.Aeson (defaultAesonOptions, mkObject, mkPair)
import CustomerIO.Track.Events.Types.Core (Timestamp)
import Data.Aeson (Object, ToJSON(toJSON), Value(..))
import Data.Aeson.TH (deriveToJSON)
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

deriveToJSON defaultAesonOptions ''Identifier
deriveToJSON defaultAesonOptions ''CioRelationships

instance ToJSON CioRelationshipAction where
  toJSON AddCioRelationships = String "add_relationships"
  toJSON RemoveCioRelationships = String "remove_relationships"

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

module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer where

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
  , aucUpdate                     :: Maybe Bool
  , aucCioRelationships           :: Maybe CioRelationships
  , aucUnsubscribed               :: Maybe Bool
  , aucCioSubscriptionPreferences :: Maybe Object -- TODO
  , aucAttributes                 :: Maybe Object
  }

data CioRelationshipAction = addCioRelationships | removeCioRelationships

data Identifier = MkIdentifier
    { iObjectTypeId :: Text
    , iObjectId :: Text
    }

data CioRelationships = MkCioRelationships
  { crAction :: CioRelationshipAction
  , crRelationships :: [Identifier]
  }

instance ToJSON CioRelationshipAction where
  toJSON addCioRelationships = String "add_relationships"
  toJSON removeCioRelationships = String "remove_relationships"

defaultAddOrUpdateCustomerBody :: AddOrUpdateCustomerBody
defaultAddOrUpdateCustomerBody = mkAddOrUpdateCustomerBody Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deriveToJSON defaultAesonOptions ''Identifier
deriveToJSON defaultAesonOptions ''CioRelationships
deriveToJSON defaultAesonOptions ''AddOrUpdateCustomerBody

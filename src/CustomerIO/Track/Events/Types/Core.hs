module CustomerIO.Track.Events.Types.Core (BasicAuthToken, Timestamp (..)) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Servant.API (BasicAuth)

type BasicAuthToken = BasicAuth "API Token" Text

newtype Timestamp
  = Timestamp { unTimestamp :: UTCTime }
  deriving stock (Show)

instance FromJSON Timestamp where
  parseJSON = fmap (Timestamp . posixSecondsToUTCTime) . parseJSON

instance ToJSON Timestamp where
  toJSON = toJSON . utcTimeToPOSIXSeconds . unTimestamp


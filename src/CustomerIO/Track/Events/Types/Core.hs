module CustomerIO.Track.Events.Types.Core (BasicAuthToken, Timestamp (..)) where

import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Servant.API (BasicAuth)

type BasicAuthToken = BasicAuth "CustomerIO" Text

newtype Timestamp
  = Timestamp { unTimestamp :: UTCTime }
  deriving newtype (Eq, Show)

instance FromJSON Timestamp where
  parseJSON = fmap (Timestamp . posixSecondsToUTCTime) . parseJSON

instance ToJSON Timestamp where
  toJSON = toJSON @Int64 . floor . utcTimeToPOSIXSeconds . unTimestamp


module CustomerIO.Track.Events.Types.Core (BasicAuthToken, Timestamp (..)) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Text (Text)
import Servant.API (BasicAuth)

type BasicAuthToken = BasicAuth "API Token" Text

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }

instance ToJSON Timestamp where
  toJSON = toJSON . utcTimeToPOSIXSeconds . unTimestamp


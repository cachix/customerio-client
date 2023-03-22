module CustomerIO.Track.Events.Types.Core (Timestamp (..)) where

import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }

instance ToJSON Timestamp where
  toJSON = toJSON . utcTimeToPOSIXSeconds . unTimestamp

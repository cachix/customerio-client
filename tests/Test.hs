{-# LANGUAGE QuasiQuotes #-}
module Main ( main ) where

import CustomerIO
import Data.Aeson
import Data.Aeson.QQ
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Exts (fromList)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "ToJSON" toJsonTests ]

toJsonTests :: [TestTree]
toJsonTests =
  [ testCase "Timestamp" $
      testToJSON (timestamp, [aesonQQ|#{timestampPosix}|])

  , testCase "AddOrUpdateCustomerBody" $
      testToJSON addOrUpdateCustomerBodyExample

  , testCase "TrackCustomerEvent" $
      testToJSON trackCustomerEventExample
  ]

-- TODO: is there a way to pretty-print the expected and actual values?
testToJSON :: (ToJSON a) => (a, Value) -> Assertion
testToJSON (example, serializedExample) = encode (toJSON example) @?= encode serializedExample

-- NOTE: the timestamp is converted to POSIX seconds.
-- When serialized to JSON, any precision beyond seconds is lost.
timestamp :: Timestamp
timestamp = Timestamp $ unsafePerformIO (iso8601ParseM "2023-07-29T02:01:32.123Z")

timestampPosix :: POSIXTime
timestampPosix = 1690596092

addOrUpdateCustomerBodyExample :: (AddOrUpdateCustomerBody, Value)
addOrUpdateCustomerBodyExample =
  ( defaultAddOrUpdateCustomerBody {
      aucEmail = Just "user@example.com",
      auc_Update = Just True,
      aucExtraAttributes = Just $ fromList ["full_name" .= ("Jane" :: Text)]
    }
  , [aesonQQ|
      { email: "user@example.com",
        _update: true,
        full_name: "Jane"
      }
    |]
  )

trackCustomerEventExample :: (TrackCustomerEventBody, Value)
trackCustomerEventExample =
  ( (defaultTrackCustomerEvent "purchased") {
      tceId = Just "123",
      tceTimestamp = Just timestamp,
      tceData = Just $ defaultCustomerEventData $ Just $ fromList [
        "foo" .= ("bar" :: Text)
      ]
    }
  , [aesonQQ|
      {
        name: "purchased",
        id: "123",
        timestamp: #{timestampPosix},
        data: { foo: "bar" }
      }
    |]
  )

{-# LANGUAGE QuasiQuotes #-}
module Main ( main ) where

import CustomerIO
import Data.Aeson
import Data.Aeson.QQ
import Data.Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Exts (fromList)
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
  [ testCase "AddOrUpdateCustomerBody" $
      testToJSON addOrUpdateCustomerBodyExample

  , testCase "TrackCustomerEvent" $
      testToJSON trackCustomerEventExample
  ]

-- TODO: is there a way to pretty-print the expected and actual values?
testToJSON :: (ToJSON a) => (a, Value) -> Assertion
testToJSON (example, serializedExample) = encode (toJSON example) @?= encode serializedExample

timestamp :: Timestamp
timestamp = Timestamp (posixSecondsToUTCTime 0)

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
        timestamp: 0,
        data: { foo: "bar" }
      }
    |]
  )

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module CustomerIO.Track.Events.Types.ReportPushMetrics
  ( ReportPushMetricsBody(..)
  , EventType(..)
  ) where

import CustomerIO.Aeson (defaultAesonOptions)
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Text (Text)

data EventType = Opened | Converted | Delivered

instance FromJSON EventType where
  parseJSON = withText "EventType" $ \case
    "opened" -> pure Opened
    "converted" -> pure Converted
    "delivered" -> pure Delivered
    x -> fail $ "Unknown event type: " <> show x

instance ToJSON EventType where
  toJSON = \case
    Opened -> "opened"
    Converted -> "converted"
    Delivered -> "delivered"

data ReportPushMetricsBody = MkReportPushMetrics
  { rpmDeliveryId :: Text
  , rpmEvent      :: EventType
  , rpmDeviceId   :: Text
  , rpmTimestamp  :: Int
  }

deriveFromJSON defaultAesonOptions ''ReportPushMetricsBody
deriveToJSON defaultAesonOptions ''ReportPushMetricsBody

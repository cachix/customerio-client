-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module Customer.Events.API (api) where

import Customer.Events.Types.ReportPushMetrics (ReportPushMetricsBody)
import Customer.Events.Types.TrackAnonymusEvent (TrackAnonymusEventBody)
import Customer.Events.Types.TrackCustomerEvent (TrackCustomerEventBody)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API (BasicAuth, Capture, JSON, Post, ReqBody, type (:<|>), type (:>))

type BasicAuthToken = BasicAuth "API Token" Text

type TrackCustomerEvent
  =  BasicAuthToken
  :> Capture "identifier" Text
  :> "events"
  :> ReqBody '[JSON] TrackCustomerEventBody
  :> Post '[JSON] ()

type TrackAnonymusEvent
  =  BasicAuthToken
  :> "events"
  :> ReqBody '[JSON] TrackAnonymusEventBody
  :> Post '[JSON] ()

type ReportPushMetrics
  = "push"
  :> "events"
  :> ReqBody '[JSON] ReportPushMetricsBody
  :> Post '[JSON] ()

type API
  = "api"
  :> "v1"
  :> ( TrackCustomerEvent
  :<|> TrackAnonymusEvent
  :<|> ReportPushMetrics
  )

api :: Proxy API
api = Proxy

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.API (module CustomerIO.Track.Events.API) where

import CustomerIO.Track.Events.Types.Core (BasicAuthToken)
import CustomerIO.Track.Events.Types.ReportPushMetrics (ReportPushMetricsBody)
import CustomerIO.Track.Events.Types.TrackAnonymousEvent (TrackAnonymousEventBody)
import CustomerIO.Track.Events.Types.TrackCustomerEvent (TrackCustomerEventBody)
import Data.Text (Text)
import Servant.API (Capture, JSON, Post, ReqBody, type (:>))


type TrackCustomerEvent
  =  BasicAuthToken
  :> "customers"
  :> Capture "identifier" Text
  :> "events"
  :> ReqBody '[JSON] TrackCustomerEventBody
  :> Post '[JSON] ()

type TrackAnonymousEvent
  =  BasicAuthToken
  :> "events"
  :> ReqBody '[JSON] TrackAnonymousEventBody
  :> Post '[JSON] ()

type ReportPushMetrics
  =  "push"
  :> "events"
  :> ReqBody '[JSON] ReportPushMetricsBody
  :> Post '[JSON] ()


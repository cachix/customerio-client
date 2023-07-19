-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

module CustomerIO.Track.Events.API (module CustomerIO.Track.Events.API) where

import CustomerIO.Track.Events.Types.ReportPushMetrics (ReportPushMetricsBody)
import CustomerIO.Track.Events.Types.TrackAnonymousEvent (TrackAnonymousEventBody)
import CustomerIO.Track.Events.Types.TrackCustomerEvent (TrackCustomerEventBody)
import CustomerIO.Track.Customer.Types.AddOrUpdateCustomer (AddOrUpdateCustomerBody)
import CustomerIO.Track.Customer.Types.AddOrUpdateCustomerDevice (AddOrUpdateCustomerDeviceBody)
import Data.Text (Text)
import Servant.API (BasicAuth, Capture, Delete, JSON, Post, Put, ReqBody, type (:<|>), type (:>))

type BasicAuthToken = BasicAuth "API Token" Text

type AddOrUpdateCustomer
  =  BasicAuthToken
  :> "customers"
  :> Capture "identifier" Text
  :> ReqBody '[JSON] AddOrUpdateCustomerBody
  :> Put '[JSON] ()

type DeleteCustomer
  =  BasicAuthToken
  :> "customers"
  :> Capture "identifier" Text
  :> Delete '[JSON] ()

type AddOrUpdateCustomerDevice
  =  BasicAuthToken
  :> "customers"
  :> Capture "identifier" Text
  :> "devices"
  :> ReqBody '[JSON] AddOrUpdateCustomerDeviceBody
  :> Put '[JSON] () -- 400: meta: errors: [string]

type DeleteCustomerDevice
  =  BasicAuthToken
  :> "customers"
  :> Capture "identifier" Text
  :> "devices"
  :> Capture "deviceId" Text
  :> Delete '[JSON] () -- 400: meta: errors: [string]

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
  = "push"
  :> "events"
  :> ReqBody '[JSON] ReportPushMetricsBody
  :> Post '[JSON] ()


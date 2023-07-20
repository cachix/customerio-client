-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}

module CustomerIO
  ( module CustomerIO
  , module CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
  , module CustomerIO.Track.Customers.Types.AddOrUpdateCustomerDevice
  , module CustomerIO.Track.Events.Types.Core
  , module CustomerIO.Track.Events.Types.ReportPushMetrics
  , module CustomerIO.Track.Events.Types.TrackAnonymousEvent
  , module CustomerIO.Track.Events.Types.TrackCustomerEvent
  ) where

import CustomerIO.Track.API (api)
import CustomerIO.Track.Customers.Types.AddOrUpdateCustomer
import CustomerIO.Track.Customers.Types.AddOrUpdateCustomerDevice
import CustomerIO.Track.Events.Types.Core
import CustomerIO.Track.Events.Types.ReportPushMetrics
import CustomerIO.Track.Events.Types.TrackAnonymousEvent
import CustomerIO.Track.Events.Types.TrackCustomerEvent
import Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import Servant.API
import Servant.Client

-- | Default host of the track API: https://track.customer.io:443
host :: BaseUrl
host = BaseUrl Https "track.customer.io" 443 ""

data Env = MkEnv
  { authtoken :: BasicAuthData
  , clientEnv :: ClientEnv
  }

-- | Same as `mkEnvDef`, but you can change BaseUrl.
--   May be useful only if `host` is outdated.
mkEnv :: BaseUrl -> BasicAuthData -> HTTP.Manager -> Env
mkEnv host' authtoken httpManager = MkEnv {..}
  where
    clientEnv = mkClientEnv httpManager host'

-- | Default way to create client environment
mkEnvDef :: BasicAuthData -> HTTP.Manager -> Env
mkEnvDef = mkEnv host

mkEnvWithClientEnv :: ClientEnv -> BasicAuthData -> Env
mkEnvWithClientEnv clientEnv authtoken = MkEnv {..}

-- Customers

addOrUpdateCustomerC :: BasicAuthData -> Text -> AddOrUpdateCustomerBody -> ClientM ()
deleteCustomerC :: BasicAuthData -> Text -> ClientM ()
addOrUpdateCustomerDeviceC :: BasicAuthData -> Text -> AddOrUpdateCustomerDeviceBody -> ClientM ()
deleteCustomerDeviceC :: BasicAuthData -> Text -> Text -> ClientM ()

trackCustomerEventC :: BasicAuthData -> Text -> TrackCustomerEventBody -> ClientM ()
trackAnonymousEventC :: BasicAuthData -> TrackAnonymousEventBody -> ClientM ()
reportPushMetricsC  :: ReportPushMetricsBody -> ClientM ()

addOrUpdateCustomerC
  :<|> deleteCustomerC
  :<|> addOrUpdateCustomerDeviceC
  :<|> deleteCustomerDeviceC
  :<|> trackCustomerEventC
  :<|> trackAnonymousEventC
  :<|> reportPushMetricsC
  = client api

addOrUpdateCustomer :: Env -> Text -> AddOrUpdateCustomerBody -> IO (Either ClientError ())
addOrUpdateCustomer MkEnv{..} identifier body = do
  runClientM (addOrUpdateCustomerC authtoken identifier body) clientEnv

deleteCustomer :: Env -> Text -> IO (Either ClientError ())
deleteCustomer MkEnv{..} identifier = do
  runClientM (deleteCustomerC authtoken identifier) clientEnv

addOrUpdateCustomerDevice :: Env -> Text -> AddOrUpdateCustomerDeviceBody -> IO (Either ClientError ())
addOrUpdateCustomerDevice MkEnv{..} identifier body = do
  runClientM (addOrUpdateCustomerDeviceC authtoken identifier body) clientEnv

deleteCustomerDevice :: Env -> Text -> Text -> IO (Either ClientError ())
deleteCustomerDevice MkEnv{..} identifier deviceId = do
  runClientM (deleteCustomerDeviceC authtoken identifier deviceId) clientEnv

trackCustomerEvent :: Env -> Text -> TrackCustomerEventBody -> IO (Either ClientError ())
trackCustomerEvent MkEnv{..} identifier body = do
  runClientM (trackCustomerEventC authtoken identifier body) clientEnv

trackAnonymousEvent :: Env -> TrackAnonymousEventBody -> IO (Either ClientError ())
trackAnonymousEvent MkEnv{..} body = do
  runClientM (trackAnonymousEventC authtoken body) clientEnv

reportPushMetrics :: Env -> ReportPushMetricsBody -> IO (Either ClientError ())
reportPushMetrics MkEnv{..} body = do
  runClientM (reportPushMetricsC body) clientEnv

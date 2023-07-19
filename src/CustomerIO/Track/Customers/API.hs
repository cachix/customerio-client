module CustomerIO.Track.Customers.API where

import Data.Text (Text)
import Servant.API (BasicAuth, Capture, JSON, Post, ReqBody, type (:<|>), type (:>))

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

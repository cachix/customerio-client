module CustomerIO.Track.Customers.API
  ( module CustomerIO.Track.Customers.API
  )
  where

import CustomerIO.Track.Customers.Types.AddOrUpdateCustomer (AddOrUpdateCustomerBody)
import CustomerIO.Track.Customers.Types.AddOrUpdateCustomerDevice (AddOrUpdateCustomerDeviceBody)
import CustomerIO.Track.Events.Types.Core (BasicAuthToken)
import Data.Text (Text)
import Servant.API (Capture, Delete, JSON, Put, ReqBody, type (:>))

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

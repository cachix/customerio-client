module CustomerIO.Track.API where

import CustomerIO.Track.Events.API
import CustomerIO.Track.Customers.API
import Data.Proxy (Proxy(..))
import Servant.API (type (:<|>), type (:>))

type API
  = "api"
  :> "v1"
  :> ( AddOrUpdateCustomer
  :<|> DeleteCustomer
  :<|> AddOrUpdateCustomerDevice
  :<|> DeleteCustomerDevice
  :<|> TrackCustomerEvent
  :<|> TrackAnonymousEvent
  :<|> ReportPushMetrics
  )

api :: Proxy API
api = Proxy

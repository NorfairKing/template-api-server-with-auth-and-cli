module Foobar.Client
  ( module Foobar.Client,
    module Foobar.API,
    module X,
  )
where

import Foobar.API
import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic

foobarClient :: FoobarRoutes (AsClientT ClientM)
foobarClient = genericClient

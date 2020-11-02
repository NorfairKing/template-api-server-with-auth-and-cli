module Foo.Bar.Client
  ( module Foo.Bar.Client,
    module Foo.Bar.API,
    module X,
  )
where

import Foo.Bar.API
import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic

fooBarClient :: FooBarRoutes (AsClientT ClientM)
fooBarClient = genericClient

module Template.Client
  ( module Template.Client,
    module Template.API,
    module X,
  )
where

import Servant.API as X
import Servant.Auth.Client as X
import Servant.Client as X
import Servant.Client.Generic
import Template.API
import Template.Data

templateClient :: TemplateRoutes (AsClientT ClientM)
templateClient = genericClient

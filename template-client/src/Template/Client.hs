module Template.Client (module Template.Client, module Template.API) where

import Servant.API
import Servant.Client
import Servant.Client.Generic
import Template.API

templateClient :: TemplateRoutes (AsClientT ClientM)
templateClient = genericClient

clientPostRegister :: RegistrationForm -> ClientM NoContent
clientPostRegister = postRegister templateClient

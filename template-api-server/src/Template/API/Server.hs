module Template.API.Server where

import Control.Monad.Reader
import Database.Persist
import Database.Persist.Sql
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Server
import Servant.Server.Generic
import Template.API as API
import Template.API.Server.Env
import Template.API.Server.Handler

templateAPIServer :: IO ()
templateAPIServer = putStrLn "Hi"

templateAPIServerApp :: Env -> Wai.Application
templateAPIServerApp env = genericServeT (flip runReaderT env) templateHandlers

templateHandlers :: TemplateRoutes (AsServerT H)
templateHandlers =
  TemplateRoutes
    { postRegister = handlePostRegister,
      postLogin = handlePostLogin
    }

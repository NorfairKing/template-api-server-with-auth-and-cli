module Foobar.CLI.Commands.Greet where

import Foobar.CLI.Commands.Import

greet :: C ()
greet = withClient $ \cenv -> withLogin cenv $ \token -> do
  resp <- runClientOrDie cenv $ getGreet foobarClient token
  liftIO $ print resp

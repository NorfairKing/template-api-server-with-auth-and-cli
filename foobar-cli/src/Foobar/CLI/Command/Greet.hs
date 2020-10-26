module Foobar.CLI.Command.Greet where

import Foobar.CLI.Command.Import

greet :: C ()
greet = withClient $ \cenv -> withLogin cenv $ \token -> do
  resp <- runClientOrDie cenv $ getGreet foobarClient token
  liftIO $ print resp

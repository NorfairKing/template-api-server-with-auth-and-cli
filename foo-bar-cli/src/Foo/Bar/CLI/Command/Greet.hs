module Foo.Bar.CLI.Command.Greet where

import Foo.Bar.CLI.Command.Import

greet :: C ()
greet = withClient $ \cenv -> withLogin cenv $ \token -> do
  resp <- runClientOrDie cenv $ getGreet fooBarClient token
  liftIO $ print resp

module Foo.Bar.CLI.Command.Login where

import Foo.Bar.CLI.Command.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()

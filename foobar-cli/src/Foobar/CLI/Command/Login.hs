module Foobar.CLI.Command.Login where

import Foobar.CLI.Command.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()

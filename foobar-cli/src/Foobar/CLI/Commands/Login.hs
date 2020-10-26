module Foobar.CLI.Commands.Login where

import Foobar.CLI.Commands.Import

login :: C ()
login = withClient $ \cenv -> withLogin cenv $ \_ ->
  pure ()

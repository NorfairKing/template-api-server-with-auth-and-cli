module Foo.Bar.API.Server.Handler.GreetSpec (spec) where

import Foo.Bar.API
import Foo.Bar.API.Server.TestUtils
import Foo.Bar.Client
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $
  describe "GetGreet" $
    it "does not crash" $
      \cenv -> withAnyNewUser cenv $ \token -> do
        resp <- testClientOrErr cenv $ getGreet fooBarClient token
        shouldBeValid resp

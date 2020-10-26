module Foobar.API.Server.Handler.GreetSpec (spec) where

import Foobar.API
import Foobar.API.Server.TestUtils
import Foobar.Client
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "GetGreet"
  $ it "does not crash"
  $ \cenv -> withAnyNewUser cenv $ \token -> do
    resp <- testClientOrErr cenv $ getGreet foobarClient token
    shouldBeValid resp

module Template.API.Server.Handler.AuthSpec (spec) where

import Template.API
import Template.API.Server.TestUtils
import Template.Client
import Template.Data
import Template.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec $ do
  describe "PostRegister" $ do
    it "does not crash" $ \cenv ->
      forAllValid $ \rf -> do
        NoContent <- testClient cenv $ postRegister templateClient rf
        pure ()

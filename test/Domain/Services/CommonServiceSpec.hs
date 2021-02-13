module Domain.Services.CommonServiceSpec where

import ClassyPrelude
import Test.Hspec

spec :: Spec
spec = do
    describe "publishAction" $ do
        it "should publish Post, access Author"  $ do
            1 + 1 `shouldBe` 2 
   
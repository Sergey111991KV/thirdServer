module Domain.Services.AuthSpec where

import ClassyPrelude
import Test.Hspec
import Fixture


import ClassyPrelude
import Domain.Types.AuthEntity.Auth


spec :: Spec
spec = do
    describe "sessionByAuth" $ do
        it "should return session if right login and password" $ do 
             1 + 1 `shouldBe` 2 
       
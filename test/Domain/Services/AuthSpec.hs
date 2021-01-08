module Domain.Services.AuthSpec where

import ClassyPrelude
import Test.Hspec
import Fixture

import Domain.Services.FixtureDomainService

import Domain.Types.AuthEntity.Auth

import Domain.Types.ImportTypes
import Domain.Services.Auth


instance Auth App where
    findUserId = dispatch2 _findUserId
    newSession = dispatch _newSession
    findUserIdBySession = dispatch _findUserIdBySession
    deleteOldSession = dispatch _deleteOldSession


spec :: Spec
spec = do
    let passwordTest = Password "passwordRight"
    let loginTest =  Login "loginRight"
    let passwordWrong = Password "passwordWrong"
    let loginWrong =  Login "loginWrong"
    let userIdTest = UserId 1
    let testSessionId = SessionId "session 1"
    describe "sessionByAuth" $ do
        it "should return session if right login and password" $ do 
            let fixture = emptyFixture { _findUserId = \loginTest passwordTest  -> return $ Right userIdTest
                                       ,  _newSession =   \userIdTest ->   return $ Right testSessionId          }
            runApp fixture (sessionByAuth loginTest passwordTest) `shouldReturn` Right testSessionId
        it "should not return session as login wrong" $ do 
            let fixture = emptyFixture { _findUserId = \loginTest passwordTest  -> return $ Left LoginErrorInvalidAuth} 
            runApp fixture (sessionByAuth loginWrong passwordTest) `shouldReturn` Left LoginErrorInvalidAuth 
        it "should not return session as password wrong" $ do 
            let fixture = emptyFixture { _findUserId = \loginTest passwordTest  -> return $ Left PasswordErrorInvalidAuth }
            runApp fixture (sessionByAuth loginTest passwordWrong) `shouldReturn` Left PasswordErrorInvalidAuth 
        it "should not eturn session as problem with create session" $ do 
            let fixture = emptyFixture { _findUserId = \_ _  -> return $ Right userIdTest
                                       ,  _newSession =   \_ ->   return $ Left NotCreateSession          }
            runApp fixture (sessionByAuth loginTest passwordTest) `shouldReturn`  Left NotCreateSession  
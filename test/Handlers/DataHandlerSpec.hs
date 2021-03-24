{-# LANGUAGE OverloadedStrings #-}
module Handlers.DataHandlerSpec where

import SpecHelper


createNewUser :: UserFull
createNewUser = UserFull{name="novo", email="novo@novo", password="novonovo", registrationDate="2015-11-19 18:28:52.607875 UTC", isAdmin=False}

isUser :: IO (Maybe UserFull) -> IO Bool
isUser mUser = do
    (Just usr) <- mUser
    let newUsr = UserFull{email=email usr, registrationDate= registrationDate usr, name=name usr, password= password usr, isAdmin= isAdmin usr}
    return $ usr == newUsr

spec :: Spec
spec = do
    describe "getUser" $ do
        it "Quando se busca um usuário que não existe" $
            getUser "naoexisto@gmail.com" `shouldReturn` Nothing
        it "Quando se busca um usuário que existe" $
            isUser (getUser "admin@admin") `shouldReturn` True

    describe "saveUser" $ do
        it "Quando se salva um usuário novo" $
            saveUser (createNewUser) `shouldReturn` True
        it "Quando se salva um usuário já existente" $
            saveUser (createNewUser) `shouldReturn` False

    describe "deleteUser" $ do
        it "Quando se remove um usuário existente" $
            deleteUser (email $ createNewUser) `shouldReturn` True
        it "Quando se remove um usuário não existente" $
            deleteUser (email $ createNewUser) `shouldReturn` False

    describe "noRoomsYet" $ do
        {-FIXME: when roomsData.json gets populated it will fail -}
        it "Quando não se tem salas cadastradas ainda" $
            noRoomsYet `shouldReturn` True



main = hspec spec
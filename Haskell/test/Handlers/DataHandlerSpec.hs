
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

createNewRoom :: Room
createNewRoom = Room{code="SalaTeste2", schedule=[], resources=[], capacity=1, localization="Bloco BL", category = Classroom}

isRoom :: IO (Maybe Room) -> IO Bool
isRoom tstroom = do
    (Just room) <- tstroom
    let newRoom = Room{code=code room, schedule=schedule room, resources=resources room, capacity=capacity room, localization=localization room, category =category room}
    return $ room == newRoom

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
        it "Quando se tem salas cadastradas" $
            noRoomsYet `shouldReturn` False

    describe "saveRoom" $ do
        it "Ao se adicionar uma sala" $
            saveRoom createNewRoom `shouldReturn` True
        it "Ao se adicionar uma sala que já existe" $
            saveRoom createNewRoom `shouldReturn` False

    describe "getRoom" $ do
        it "Ao se tentar acessar uma sala que não existe" $
            getRoom "LCC-2" `shouldReturn` Nothing
        it "Ao se acessar uma sala que existe" $
            isRoom (getRoom "SalaTeste2") `shouldReturn` True
    
    describe "deleteRoom" $ do
        it "Ao se remover uma sala que existe" $
            deleteRoom "SalaTeste2" `shouldReturn` True
        it "Ao se remover uma sala que não existe" $
            deleteRoom "SalaTeste2" `shouldReturn` False

        
main = hspec spec
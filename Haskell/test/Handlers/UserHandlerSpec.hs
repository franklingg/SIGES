module Handlers.UserHandlerSpec where

import SpecHelper

userTeste :: User
userTeste = User "testador" "teste@teste.com" False

spec :: Spec
spec = do
    describe "userExists" $ do
        it "Quando é um usuário existente" $
            userExists "admin@admin" `shouldReturn` True
        it "Quando é um usuário não existente" $
            userExists "nao@existo.com" `shouldReturn` False
    describe "correctPassword" $ do
        it "Senha correta" $ do
            correctPassword "admin@admin" "adminadmin" `shouldReturn` True
        it "Senha incorreta" $ do
            correctPassword "admin@admin" "naoadmin" `shouldReturn` False
    describe "newUser" $ do
        it "Cadastro com sucesso" $ do
            registerNewUser "teste@teste.com" "teste" "testador" False `shouldReturn` ()
        let user = retrieveUser "teste@teste.com"
        it "Mostrar usuário" $ do
            (user <&> show) `shouldReturn` show userTeste
        it "Remove com sucesso" $ do
            (removeUser =<< user) `shouldReturn` ()



main = hspec spec
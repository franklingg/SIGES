module Handlers.ErrorHandlerSpec where

import SpecHelper

spec :: Spec
spec = do 
    describe "checkNewEmail" $ do
        it "Quando o usuário insere a tecla de retornar" $
            checkNewEmail "r" `shouldBe` Right "Retornar"
        it "Erro quando o usuário insere e-mail vazio" $
            checkNewEmail "" `shouldBe` Left "Email inválido. Tente novamente."
        it "Erro quando o usuário insere e-mail sem @" $
            checkNewEmail "teste.bol.com" `shouldBe` Left "Email inválido. Tente novamente."
        it "Erro quando o usuário insere e-mail com espaços" $
            checkNewEmail "teste.  e s@bol.com" `shouldBe` Left "Email inválido. Tente novamente."
        it "Quando o usuário insere um e-mail correto" $
            checkNewEmail "teste.unit@bol.com" `shouldBe` Right "teste.unit@bol.com"
    describe "checkEmail" $ do
        it "Wrong email" $ 
            checkEmail "naoexisto@gmail.com" `shouldReturn` Left "Email não cadastrado. Tente novamente."
        {-FIXME: it "Right email" $ 
            pendingWith "Change when UserHandler is implemented"-}

    describe "checkNewPassword" $ do
        it "Erro quando o usuário digita uma senha pequena" $
            checkNewPassword "1234567" `shouldBe` Left "A senha deve ter mais de 8 caracteres. Tente novamente."
        it "Erro quando o usuário digita uma senha vazia" $
            checkNewPassword "" `shouldBe` Left "A senha deve ter mais de 8 caracteres. Tente novamente."
        it "Quando o usuário digita uma senha correta" $
            checkNewPassword "eutenhomaisde8caracteres" `shouldBe` Right "eutenhomaisde8caracteres"
    describe "checkPassword" $ do
        it "Wrong password" $
            pendingWith "Change when UserHandler is implemented"
        it "Right password" $
            pendingWith "Change when UserHandler is implemented"


main = hspec spec
module Handlers.ErrorHandlerSpec where

import SpecHelper

spec :: Spec
spec = do 
    describe "checkNewEmail" $ do
        it "Quando o usuário insere a tecla de retornar" $
            checkNewEmail "r" `shouldReturn` Right "Retornar"
        it "Erro quando o usuário insere e-mail vazio" $
            checkNewEmail "" `shouldReturn` Left "Email inválido. Tente novamente."
        it "Erro quando o usuário insere e-mail sem @" $
            checkNewEmail "teste.bol.com" `shouldReturn` Left "Email inválido. Tente novamente."
        it "Erro quando o usuário insere e-mail com espaços" $
            checkNewEmail "teste.  e s@bol.com" `shouldReturn` Left "Email inválido. Tente novamente."
        it "Quando o usuário insere um e-mail correto" $
            checkNewEmail "teste.unit@bol.com" `shouldReturn` Right "teste.unit@bol.com"
    describe "checkEmail" $ do
        it "Wrong email" $ 
            checkEmail "naoexisto@gmail.com" `shouldReturn` Left "Email não cadastrado. Tente novamente."
        it "Right email" $ 
            checkEmail "admin@admin" `shouldReturn` Right "admin@admin"

    describe "checkNewPass" $ do
        it "Erro quando o usuário digita uma senha pequena" $
            checkNewPass "1234567" `shouldReturn` Left "A senha deve ter mais de 8 caracteres. Tente novamente."
        it "Erro quando o usuário digita uma senha vazia" $
            checkNewPass "" `shouldReturn` Left "A senha deve ter mais de 8 caracteres. Tente novamente."
        it "Quando o usuário digita uma senha correta" $
            checkNewPass "eutenhomaisde8caracteres" `shouldReturn` Right "eutenhomaisde8caracteres"
    describe "checkPass" $ do
        it "Wrong password" $
            checkPass "admin@admin" "senhaincorreta" `shouldReturn` Left "Senha incorreta. Tente novamente."
        it "Right password" $
            checkPass "admin@admin" "adminadmin" `shouldReturn` Right "adminadmin"


main = hspec spec
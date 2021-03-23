module Handlers.DataHandlerSpec where

import Manager
import SpecHelper

spec :: Spec
spec = do
    describe "getUser" $ do
        it "Quando se busca um usuário que não existe" $
            getUser "naoexisto@gmail.com" `shouldReturn` Nothing
        {-it "Quando se busca um usuário que existe" $
            getUser "emailTeste" `shouldReturn` (Just UserFull{email="emailTeste"
                                                              ,registrationDate="dataTeste"
                                                              ,name="nomeTeste"
                                                              ,password="senhaTeste"})-}
    describe "noUserYet" $ do
        it "Se nenhum usuário estiver cadastrado" $
            noUserYet `shouldReturn` True



main = hspec spec
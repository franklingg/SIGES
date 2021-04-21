{-# LANGUAGE OverloadedStrings #-}
module Handlers.UserHandler where

-- importa Text
import qualified Data.Text as T

-- importa a entidade Manager
import Manager
-- importa a entidade DataHandler
import Handlers.DataHandler

-- user e uma instancia da classe de tipo Show
instance Show User where
    show (User nameUsr emailUsr admUsr) = "Dados do usuário:       \n\
                                          \Nome: "++ nameUsr ++   "\n\
                                          \Email: "++ emailUsr ++ "\n\
                                          \Administrador: "++ if admUsr then "Sim" else "Não"

{-
   Funcao para verificar se o usuario existe.
-}
userExists :: String -> IO Bool
userExists emailStr = do
    possibleUser <- getUser emailStr
    return (isJust possibleUser)

{-
   Funcao para fazer a senha.
-}
makePass :: String -> Password
makePass passStr = mkPassword $ T.pack passStr

{-
   Funcao para verificar se a senha esta correta.
-}
correctPassword :: String -> String -> IO Bool
correctPassword emailStr passStr = do
    (Just user) <- getUser emailStr
    let passHash = PasswordHash (password user) :: PasswordHash Bcrypt
        check = checkPassword (makePass passStr) passHash
    return (check == PasswordCheckSuccess)

{-
   Funcao para recuperar um usuario.
-}
retrieveUser :: String -> IO (User)
retrieveUser emailStr = do
    (Just user) <- getUser emailStr
    return $ User {nameUser= name user, emailUser= email user, isAdminUser = isAdmin user}

{-
   Funcao para registrar um novo usuario.
-}
registerNewUser :: String -> String -> String -> Bool -> IO ()
registerNewUser emailStr passwordStr nameStr isAdm = do
    timenow <- getCurrentTime
    passHash <- hashPassword $ makePass passwordStr
    let passwordText = unPasswordHash passHash
        newUser = UserFull {email=emailStr, password=passwordText, name=nameStr, isAdmin=isAdm, registrationDate=(show timenow)}
    saveUser newUser
    return ()

{-
   Funcao para remover um usuario.
-}
removeUser :: User -> IO ()
removeUser user = do
    deleteUser (emailUser user)
    return ()
{-# LANGUAGE OverloadedStrings #-}
module Handlers.UserHandler where

import qualified Data.Text as T

import Manager
import Handlers.DataHandler

instance Show User where
    show (User nameUsr emailUsr admUsr) = "Dados do usuário:       \n\
                                          \Nome: "++ nameUsr ++   "\n\
                                          \Email: "++ emailUsr ++ "\n\
                                          \Administrador: "++ if admUsr then "Sim" else "Não"

userExists :: String -> IO Bool
userExists emailStr = do
    possibleUser <- getUser emailStr
    return (isJust possibleUser)

makePass :: String -> Password
makePass passStr = mkPassword $ T.pack passStr

correctPassword :: String -> String -> IO Bool
correctPassword emailStr passStr = do
    (Just user) <- getUser emailStr
    let passHash = PasswordHash (password user) :: PasswordHash Bcrypt
        check = checkPassword (makePass passStr) passHash
    return (check == PasswordCheckSuccess)

retrieveUser :: String -> IO (User)
retrieveUser emailStr = do
    (Just user) <- getUser emailStr
    return $ User {nameUser= name user, emailUser= email user, isAdminUser = isAdmin user}

registerNewUser :: String -> String -> String -> Bool -> IO ()
registerNewUser emailStr passwordStr nameStr isAdm = do
    c <- getCurrentTime
    passHash <- hashPassword $ makePass passwordStr
    let passwordText = unPasswordHash passHash
        newUser = UserFull {email=emailStr, password=passwordText, name=nameStr, isAdmin=isAdm, registrationDate=c}
    saveUser newUser
    return ()

removeUser :: User -> IO ()
removeUser user = do
    deleteUser (emailUser user)
    return ()
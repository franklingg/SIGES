{-|
Module      : UserHandler
Description : Módulo contendo as operações básicas de manipulação de usuários no sistema SIGES.
-}
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

-- | Esta função considera uma String e decide se um usuário com o e-mail igual a esta String existe no sistema.
userExists :: String -> IO Bool
userExists emailStr = do
    possibleUser <- getUser emailStr
    return (isJust possibleUser)

-- | Dada uma String, esta função a converterá em um Password.
makePass :: String -> Password
makePass passStr = mkPassword $ T.pack passStr

-- | Esta função considerará uma String contendo um e-mail e avaliará outra String, decidindo se ela equivale à senha do usuário com o e-mail fornecido. A resposta é então retornada em forma de valor booleano.
correctPassword :: String -> String -> IO Bool
correctPassword emailStr passStr = do
    (Just user) <- getUser emailStr
    let passHash = PasswordHash (password user) :: PasswordHash Bcrypt
        check = checkPassword (makePass passStr) passHash
    return (check == PasswordCheckSuccess)

-- | Dada uma String contendo um e-mail, esta função procurará no sistema um Userfull com este e-mail, e retornará a sua versão com as informações essenciais para o funcionamento do sistema, omitindo dados sensíveis: um User.
retrieveUser :: String -> IO User
retrieveUser emailStr = do
    (Just user) <- getUser emailStr
    return $ User {nameUser= name user, emailUser= email user, isAdminUser = isAdmin user}

-- | Dadas três Strings contendo respectivamente o e-mail, a senha e o nome do usuário, e um valor booleano indicando se trata-se de um administrador, esta função cria um novo usuário e o armazena no sistema.
registerNewUser :: String -> String -> String -> Bool -> IO ()
registerNewUser emailStr passwordStr nameStr isAdm = do
    timenow <- getCurrentTime
    passHash <- hashPassword $ makePass passwordStr
    let passwordText = unPasswordHash passHash
        newUser = UserFull {email=emailStr, password=passwordText, name=nameStr, isAdmin=isAdm, registrationDate=(show timenow)}
    saveUser newUser
    return ()

-- | Esta função recebe um usuário e o remove do sistema.
removeUser :: User -> IO ()
removeUser user = do
    deleteUser (emailUser user)
    return ()
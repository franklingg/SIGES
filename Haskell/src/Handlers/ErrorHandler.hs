{-# LANGUAGE OverloadedStrings #-}
module Handlers.ErrorHandler where

-- importa o Map
import qualified Data.Map as Map
-- importa o ByteStrings.Char8
import qualified Data.ByteString.Char8 as BS
-- importa o Text
import qualified Data.Text as T

-- importa a entidade Manager
import Manager
-- importa a entidade UserHandler
import Handlers.UserHandler
-- importa a entidade OutputScreens
import TUI.OutputScreens

{-
   Funcao para verificar se a tela escolhida e valida.
-}
validScreen :: Screen -> String -> IO (Either ErrorLog Screen)
validScreen screen answer
    | length answer /= 1 = return (Left "Por favor, digite apenas um caractere.")
    | not $ charAnswer `Map.member` allScreens = return (Left "Por favor, digite apenas uma opções acima.")
    | otherwise = return (Right correctScreen)
    where charAnswer = toUpper $ head answer
          allScreens = nextScreens screen
          (Just correctScreen) = Map.lookup charAnswer allScreens 

{-
   Funcao para verificar se o caractere digitado e o 'R'.
-}
singleCharIsR :: String -> Bool
singleCharIsR str
    | length str /= 1 = False
    | toUpper (head str) == 'R' = True
    | otherwise = False

{-
   Funcao para verificar o novo email.
-}
checkNewEmail :: String -> IO (Either ErrorLog String)
checkNewEmail emailString = do
    checkedEmail <- checkEmail emailString
    return (case checkedEmail of 
                (Right "Retornar") -> checkedEmail
                (Left "Email inválido. Tente novamente.") -> Left "Email inválido. Tente novamente."
                (Left notRegistered) -> Right emailString
                _ -> Left "Este e-mail já pertence a uma conta no SIGES. Tente novamente.")

{-
   Funcao para verificar o email.
-}
checkEmail :: String -> IO (Either ErrorLog String)
checkEmail emailString = do
    if singleCharIsR emailString
        then return (Right "Retornar")
        else if not $ isValid (BS.pack emailString)
            then return (Left "Email inválido. Tente novamente.")
            else (do
                checkedUser <- userExists emailString
                case checkedUser of
                    True -> return (Right emailString)
                    False -> return (Left "Email não cadastrado. Tente novamente.")
                )

{-
   Funcao para verificar a nova senha.
-}
checkNewPass :: String -> IO (Either ErrorLog String)
checkNewPass passwordString
    | not $ isValidPassword defaultPasswordPolicy_ password = return (Left "A senha deve ter mais de 8 caracteres. Tente novamente.")
    | otherwise = return (Right passwordString)
    where password = mkPassword $ T.pack passwordString

{-
   Funcao para verificar a senha.
-}
checkPass :: String -> String -> IO (Either ErrorLog String)
checkPass emailString passwordString = do
    possiblePassword <- correctPassword emailString passwordString
    case possiblePassword of
        True -> return (Right passwordString)
        False -> return (Left "Senha incorreta. Tente novamente.")

{-
   Funcao para verificar o nome.
-}
checkName :: String -> IO (Either ErrorLog String)
checkName str = do
    if all (\c -> isAlpha c || isSpace c) str
        then return (Right str)
        else return (Left "Apenas letras são permitidas. Tente novamente.")

{-
   Funcao para verificar se os caracteres digitados sao 'S' ou 'N'.
-}
yesOrNo :: String -> IO (Either ErrorLog Bool)
yesOrNo str = do
    let firstChar = toUpper $ head str
    if (length str /= 1)
        then return (Left "Resposta inválida (apenas S/N). Tente novamente.")
        else return (case firstChar of
                        'S' -> Right True
                        'N' -> Right False
                        _ -> Left "Resposta inválida (apenas S/N). Tente novamente.")
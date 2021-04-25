{-|
Module      : ErrorHandler
Description : Módulo contendo operações de verificação e tratamento de erros do sistema SIGES.
-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.ErrorHandler where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Manager
import Handlers.UserHandler
import Handlers.DataHandler
import TUI.OutputScreens


-- | Dada uma determinada tela do sistema e uma resposta, esta função verifica se a resposta foi adequada àquela tela, retornando, em caso positivo, a tela equivalente à resposta, ou em caso negativo, o ErrorLog com a mensagem identificando qualerro existente na resposta.
validScreen :: Screen -> String -> IO (Either ErrorLog Screen)
validScreen screen answer
    | length answer /= 1 = return (Left "Por favor, digite apenas um caractere.")
    | not $ charAnswer `Map.member` allScreens = return (Left "Por favor, digite apenas uma opções acima.")
    | otherwise = return (Right correctScreen)
    where charAnswer = toUpper $ head answer
          allScreens = nextScreens screen
          (Just correctScreen) = Map.lookup charAnswer allScreens 

-- | Dada uma String, esta função verificará se esta String é composta unicamente pelo caractere 'R', respondendo com um valor booleano.
singleCharIsR :: String -> Bool
singleCharIsR str
    | length str /= 1 = False
    | toUpper (head str) == 'R' = True
    | otherwise = False

-- | Esta função considerará um e-mail, em String, e decidirá se ele é válido e livre ou se é inválido ou se já está em uso, retornando uma mensagem de erro caso o e-mail não seja válido e livre.
checkNewEmail :: String -> IO (Either ErrorLog String)
checkNewEmail emailString = do
    checkedEmail <- checkEmail emailString
    return (case checkedEmail of 
                (Right "Retornar") -> checkedEmail
                (Left "Email inválido. Tente novamente.") -> Left "Email inválido. Tente novamente."
                (Left notRegistered) -> Right emailString
                _ -> Left "Este e-mail já pertence a uma conta no SIGES. Tente novamente.")

-- | Esta função considerará um e-mail, em String, e decidirá se ele é válido e está cadastrado. Caso não seja válido ou não esteja cadastrado, retornará uma mensagem de erro. Caso a função receba como parâmetro uma String composta apenas da letra R, responderá com a String "Retornar".
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

-- | Esta função considerará uma String determinada e decidirá se esta String pode ser usada como senha neste sistema. Em caso negativo, retornará uma mensagem de erro.
checkNewPass :: String -> IO (Either ErrorLog String)
checkNewPass passwordString
    | not $ isValidPassword defaultPasswordPolicy_ password = return (Left "A senha deve ter mais de 8 caracteres. Tente novamente.")
    | otherwise = return (Right passwordString)
    where password = mkPassword $ T.pack passwordString

-- | Esta função considera um e-mail e uma senha, e decide se a senha está correta (caso em que retorna a própria senha) ou não (Caso em que retorna a mensagem de erro correspondente.)
checkPass :: String -> String -> IO (Either ErrorLog String)
checkPass emailString passwordString = do
    possiblePassword <- correctPassword emailString passwordString
    case possiblePassword of
        True -> return (Right passwordString)
        False -> return (Left "Senha incorreta. Tente novamente.")

-- | Esta função considera uma String e decide se ela pode ser considerada como um nome do usuário (apenas letras e espaços são permitidos). Caso a String contenha um nome válido, o próprio nome será retornado. Caso contrário, uma mensagem de erro será retornada.
checkName :: String -> IO (Either ErrorLog String)
checkName str = do
    if all (\c -> isAlpha c || isSpace c) str
        then return (Right str)
        else return (Left "Apenas letras são permitidas. Tente novamente.")

-- | Esta função considera uma String e decide se ela representa corretamente uma resposta de Sim ou Não (S ou N). Caso tenha mais de um caractere ou não seja S ou N, retornará uma mensagem de erro. Caso seja válida, a função retornará um Booleano. (True para S e False para N).
yesOrNo :: String -> IO (Either ErrorLog Bool)
yesOrNo str = do
    let firstChar = toUpper $ head str
    if (length str /= 1)
        then return (Left "Resposta inválida (apenas S/N). Tente novamente.")
        else return (case firstChar of
                        'S' -> Right True
                        'N' -> Right False
                        _ -> Left "Resposta inválida (apenas S/N). Tente novamente.")

-- | Esta função considera uma Sting e decide se ela equivale ao código de uma das salas cadastradas no sistema (Caso em que retornará o próprio código). Caso contrário, a função retornará uma mensagem de erro.
checkRoomCode :: String -> IO (Either ErrorLog String)
checkRoomCode codeStr = do
    let codeRoom = map toUpper codeStr
    possibleRoom <- getRoom codeRoom
    if isJust possibleRoom
        then return (Right codeRoom)
        else return (Left "Sala não cadastrada. Tente novamente.")

-- | Esta função considera uma String, e decidirá se ela equivale a uma data válida. Em caso positivo, retornará o mesmo na forma de uma lista de inteiros. Caso contrário, retornará uma mensagem de erro equivalente ao problema encontrado.
checkDay :: String -> IO (Either ErrorLog [Int])
checkDay dayStr
    | length dayRemovedWs /= 10 = return (Left "Formato inválido (apenas DD-MM-AAAA). Tente novamente.")
    | day > 31 || day <= 0 = return (Left "Formato inválido (apenas DD-MM-AAAA). Tente novamente.")
    | month > 12 || month <= 0 = return (Left "Formato inválido (apenas DD-MM-AAAA). Tente novamente.")
    | otherwise = return $ Right dayList
    where dayRemovedWs = foldr (\c acc -> if c /= ' ' then c:acc else acc) "" dayStr
          dayList@[day, month, year] = map read $ splitOn "-" dayRemovedWs

-- | Esta função considera uma String e decide se ela equivale a um horário. Em caso positivo, retornará o horário em forma de Lista de inteiros. Caso contrário, retornará a mensagem de erro equivalente ao problema encontrado.
checkTime :: String -> IO (Either ErrorLog [Int])
checkTime timeStr
    | length timeRemovedWs /= 5 = return (Left "Formato inválido (apenas HH:MM). Tente novamente.")
    | hours >= 24 || hours < 0 = return (Left "Formato inválido (apenas HH:MM). Tente novamente.")
    | minutes >= 60 || minutes < 0 = return (Left "Formato inválido (apenas HH:MM). Tente novamente.")
    | otherwise = return $ Right timeList
    where timeRemovedWs = foldr (\c acc -> if c /= ' ' then c:acc else acc) "" timeStr
          timeList@[hours, minutes] = map read $ splitOn ":" timeRemovedWs

-- | Esta função considera uma String e decide se ela pode ser usada como descrição para uma reserva de sala. Em caso positivo, retornará a mesma String. Caso contrário, retornará uma mensagem de erro.
checkDescription :: String -> IO (Either ErrorLog String)
checkDescription str = do
    if all (\c -> isPrint c) str && length str > 5
        then return (Right str)
        else return (Left "Apenas letras são permitidas. Tente novamente.")

-- | Esta função considera uma String e decide se ela representa uma das categorias no sistema. Caso represente, a função retornará a Categoria desejada, e em caso contrário, retornará uma mensagem de erro.
checkCategory :: String -> IO (Either ErrorLog RoomCategory)
checkCategory catStr
    | c == 'L' = return $ Right Laboratory
    | c == 'A' = return $ Right Auditorium
    | c == 'S' = return $ Right Classroom
    | c == 'E' = return $ Right Office
    | c == 'D' = return $ Right Warehouse
    | otherwise = return $ Left "Apenas uma das opções acima. Tente novamente"
    where c = toUpper (head catStr) 

-- | Esta função considera uma String e decide se ela contém um valor inteiro entre zero e trezentos, caso no qual retornará o inteiro. Em caso contrário, uma mensagem de erro será retornada.
checkNumber :: String -> IO (Either ErrorLog Int)
checkNumber numStr
    | num > 0 && num < 300 = return $ Right num
    | otherwise = return $ Left "Valor inválido. Tente novamente"
    where num = read numStr

-- | Esta função considera uma String e decide se ela representa um dos tipos de recursos oferecidos pelas salas do sistema. Caso afirmativo, a função retornará o ResourceKind solicitado. Caso contrário, uma mensagem de erro será retornada.
checkResource :: String -> IO (Either ErrorLog ResourceKind)
checkResource resStr
    | c == 'P' = return $ Right Projector
    | c == 'M' = return $ Right Microscope
    | c == 'B' = return $ Right Desk
    | c == 'C' = return $ Right Computer
    | c == 'Q' = return $ Right Board
    | c == 'A' = return $ Right AirConditioner
    | otherwise = return $ Left "Apenas uma das opções acima. Tente novamente"
    where c = toUpper (head resStr) 

-- | Esta função considera uma }String e decide se ela equivale a uma das opções (1, 2, 3, ou 4), caso em que retorna a opção como um inteiro. Em caso contrário, retornará uma mensagem de erro.
checkFilter :: String -> IO (Either ErrorLog Int)
checkFilter filterStr = do
    let filterNum = read filterStr
    if filterNum `elem` [1,2,3,4] 
        then return $ Right filterNum 
        else return $ Left "Valor inválido. Tente novamente"
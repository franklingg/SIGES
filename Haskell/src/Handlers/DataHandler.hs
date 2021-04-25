{-|
Module      : DataHandler
Description : Módulo contendo as operações de manipulação de dados e persistência do sistema SIGES.
Copyright   : 
-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.DataHandler where

import qualified Data.ByteString.Lazy as BL

import Manager

-- |Esta função tem o valor igual ao caminho para o arquivo json com os dados temporários de usuário.
userTmpJSON :: FilePath
userTmpJSON = "data/userTemp.json"

-- |Esta função tem o valor igual ao caminho para o arquivo json com os dados de usuário.
userJSON :: FilePath
userJSON = "data/userData.json"

-- |Dado um FilePath, esta função verifica se ele se refere a um arquivo existente.
fileExists :: FilePath -> IO (Bool)
fileExists path = do doesFileExist path

instance FromJSON User where
instance ToJSON User where

instance FromJSON UserFull where
instance ToJSON UserFull where

-- |Esta função verifica os usuários cadastrados no sistema e retornará um valor booleano true caso o sistema não possua nenhum usuário cadastrado ainda, e false em caso contrário.
noUsersYet :: IO Bool
noUsersYet = do
   (Just existingUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   return (null existingUsers)

-- |Esta função considera um UserFull, e caso o seu e-mail já não esteja cadastrado no sistema,. ela o incluirá no sistema, retornando um valor booleano indicando se foi possível incluir o novo usuário.
saveUser :: UserFull -> IO Bool
saveUser newUser = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == email newUser) allUsers
   if null correspondingUsers
      then do {BL.writeFile userJSON $ encode (allUsers ++ [newUser]); return True}
      else return False

-- |Esta função considera uma String, e caso ela corresponda ao e-mail de um dos usuários, a função eliminará o usuário equivalente do sistema, retornando um valor booleano indicando se a remoção pôde ser feita.
deleteUser :: String -> IO Bool
deleteUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let allButOne = filter (\user -> email user /= emailStr) allUsers
   BL.writeFile userJSON $ encode allButOne
   return $ not (allUsers == allButOne) -- If both arrays are equal, then nothing was removed

-- |Esta função considerará uma String, e retornará o usuário cadastrado no sistema com o e-mail igual a esta String, caso exista.
getUser :: String -> IO (Maybe UserFull)
getUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == emailStr) allUsers
   if null correspondingUsers
      then return Nothing
      else return $ Just (head correspondingUsers)

-- |Esta função considerará uma String, e verificará o sistema para buscar o usuário cadastrado no sistema com o e-mail igual à String. O UserFull será então incluído, como Uswer (Que contém menos informações) nos dados temporários do sistema. Estes dados poderão então ser acessados em tempo de execução do sistema.
signUser :: String -> IO ()
signUser emailStr = do
   (Just userfull) <- getUser emailStr
   let user = User (name userfull) (email userfull) (isAdmin userfull)
   BL.writeFile userTmpJSON $ encode user

-- |Esta função decidirá se algum usuário está atualmente logado no sistema.
hasLoggedUser :: IO Bool
hasLoggedUser = do fileExists userTmpJSON

-- |Esta função retornará o Usuário atualmente logado no sistema.
getLoggedUser :: IO (User)
getLoggedUser = do
   tmpUser <- decode <$> BL.readFile userTmpJSON
   if isJust tmpUser
      then return (fromJust tmpUser)
      else getLoggedUser

-- |Esta função deslogará um usuário do sistema, destruindo as informações salvas temporariamente.
signOutUser :: IO ()
signOutUser = do
   existsTmp <- hasLoggedUser
   when existsTmp $ removeFile userTmpJSON

-- |Esta função tem o valor igual ao caminho para o arquivo json com os dados das salas.
roomsJSON :: FilePath
roomsJSON = "data/roomsData.json"

instance FromJSON ResourceKind where
instance ToJSON ResourceKind where

instance FromJSON Resource where
instance ToJSON Resource where

instance FromJSON RoomCategory where
instance ToJSON RoomCategory where

instance FromJSON Reservation where
instance ToJSON Reservation where

instance FromJSON Room where
instance ToJSON Room where

-- |Esta função retorna uma lista contendo todas as salas cadastradas no sistema.
fetchRooms :: IO [Room]
fetchRooms = do
   (Just allRooms) <- decode <$> BL.readFile roomsJSON :: IO (Maybe [Room])
   return allRooms

-- |Esta função verifica as salas cadastradas no sistema e retornará um valor booleano true caso o sistema não possua nenhuma sala cadastrada ainda, e false em caso contrário.
noRoomsYet :: IO Bool
noRoomsYet = do null <$> fetchRooms

-- |Esta função considera uma sala, e caso ela ainda não exista no sistema, será incluída e um valor booleano true será retornado. Caso contrário, um valor booleano false será retornado.
saveRoom :: Room -> IO Bool
saveRoom newRoom = do
   allRooms <- fetchRooms
   let correspondingRooms = filter (\room -> code room == code newRoom) allRooms
   if null correspondingRooms
      then do {BL.writeFile roomsJSON $ encode (allRooms ++ [newRoom]); return True}
      else return False

-- |Esta função usa uma String contendo um código de sala e uma sala nova para substituir a sala com o código dado pela versão nova da mesma sala.
updateRoom :: String -> Room -> IO Bool
updateRoom codeRoom newRoom = do
   operate <- deleteRoom codeRoom
   saveRoom newRoom

-- |Esta função recebe uma função e atualiza todas as salas do sistema aplicando sobre elas a função determinada.
updateAllRooms :: (Room -> Room) -> IO Bool
updateAllRooms function = do
   allRooms <- fetchRooms
   let updated = map function allRooms
   BL.writeFile roomsJSON $ encode updated
   return True

-- |Esta função considera uma String, e caso ela corresponda ao código de uma das salas, a função eliminará a sala equivalente do sistema, retornando um valor booleano indicando se a remoção pôde ser feita.
deleteRoom :: String -> IO Bool
deleteRoom codeStr = do
   allRooms <- fetchRooms
   let removed = filter (\room -> code room /= codeStr) allRooms
   if allRooms /= removed
      then do {BL.writeFile roomsJSON $ encode removed; return True}
      else return False

-- |Esta função considerará uma String, e retornará a sala cadastrada no sistema com o código igual a esta String, caso exista.
getRoom :: String -> IO (Maybe Room)
getRoom codeStr = do
   allRooms <- fetchRooms
   let corresponding = filter (\room -> code room == codeStr) allRooms
   if null corresponding
      then return Nothing
      else return $ Just (head corresponding)

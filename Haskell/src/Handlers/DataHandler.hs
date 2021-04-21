{-# LANGUAGE OverloadedStrings #-}
module Handlers.DataHandler where

-- importa o ByteStrings
import qualified Data.ByteString.Lazy as BL

-- importa a entidade Manager
import Manager

userTmpJSON :: FilePath
userTmpJSON = "data/userTemp.json"
{-
   Funcao para carregar os dados dos usuarios cadastrados.
-}
userJSON :: FilePath
userJSON = "data/userData.json"

fileExists :: String -> IO ()
fileExists << doesFileExist

-- User e uma instancia da classe de tipo FromJSON
instance FromJSON User where
-- User e uma instancia da classe de tipo ToJSON
instance ToJSON User where

-- UserFull e uma instancia da classe de tipo FromJSON
instance FromJSON UserFull where
-- UserFull e uma instancia da classe de tipo ToJSON
instance ToJSON UserFull where

{-
   Funcao para verificar se possui usuarios cadastrados.
-}
noUsersYet :: IO Bool
noUsersYet = do
   (Just existingUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   return (null existingUsers)

{-
   Funcao para salvar um novo usuario cadastrado.
-}
saveUser :: UserFull -> IO Bool
saveUser newUser = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == email newUser) allUsers
   if null correspondingUsers
      then do {BL.writeFile userJSON $ encode (allUsers ++ [newUser]); return True}
      else return False

{-
   Funcao para deletar um usuario cadastrado.
-}
deleteUser :: String -> IO Bool
deleteUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let allButOne = filter (\user -> email user /= emailStr) allUsers
   BL.writeFile userJSON $ encode allButOne
   return $ not (allUsers == allButOne) -- If both arrays are equal, then nothing was removed

{-
   Funcao para obter os usuarios cadastrados.
-}
getUser :: String -> IO (Maybe UserFull)
getUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == emailStr) allUsers
   if null correspondingUsers
      then return Nothing
      else return $ Just (head correspondingUsers)

signUser :: String -> IO ()
signUser emailStr = do
   (Just userfull) <- getUser emailStr
   let user = User (name userfull) (email userfull) (isAdmin userfull)
   BL.writeFile userTmpJSON $ encode user

getLoggedUser :: IO (User)
getLoggedUser = do
   tmpUser <- decode <$> BL.readFile userTmpJSON
   if isJust tmpUser
      then return (fromJust tmpUser)
      else getLoggedUser

signOutUser :: IO ()
signOutUser = do
   fileExists <- doesFileExist userTmpJSON
   when fileExists $ removeFile userTmpJSON

{-
   Funcao para carregar os dados das salas cadastradas.
-}
roomsJSON :: FilePath
roomsJSON = "data/roomsData.json"

-- ResourceKind e uma instancia da classe de tipo FromJSON
instance FromJSON ResourceKind where
-- ResourceKind e uma instancia da classe de tipo ToJSON
instance ToJSON ResourceKind where

-- Resource e uma instancia da classe de tipo FromJSON
instance FromJSON Resource where
-- Resource e uma instancia da classe de tipo ToJSON
instance ToJSON Resource where

-- RoomCategory e uma instancia da classe de tipo FromJSON
instance FromJSON RoomCategory where
-- RoomCategory e uma instancia da classe de tipo ToJSON
instance ToJSON RoomCategory where

-- Reservation e uma instancia da classe de tipo FromJSON
instance FromJSON Reservation where
-- Reservation e uma instancia da classe de tipo ToJSON
instance ToJSON Reservation where

-- Room e uma instancia da classe de tipo FromJSON
instance FromJSON Room where
-- Room e uma instancia da classe de tipo ToJSON
instance ToJSON Room where

{-
   Funcao para buscar os dados das salas cadastradas.
-}
fetchRooms :: IO [Room]
fetchRooms = do
   (Just allRooms) <- decode <$> BL.readFile roomsJSON :: IO (Maybe [Room])
   return allRooms

{-
   Funcao para verificar se possui salas cadastradas.
-}
noRoomsYet :: IO Bool
noRoomsYet = do null <$> fetchRooms

{-
   Funcao para salvar uma nova sala cadastrada.
-}
saveRoom :: Room -> IO Bool
saveRoom newRoom = do
   allRooms <- fetchRooms
   let correspondingRooms = filter (\room -> code room == code newRoom) allRooms
   if null correspondingRooms
      then do {BL.writeFile roomsJSON $ encode (allRooms ++ [newRoom]); return True}
      else return False

{-
   Funcao para atualizar uma sala cadastrada.
-}
updateRoom :: String -> Room -> IO Bool
updateRoom codeRoom newRoom = do
   operate <- deleteRoom codeRoom
   saveRoom newRoom

{-
   Funcao para atualizar todas as salas cadastradas.
-}  
updateAllRooms :: (Room -> Room) -> IO Bool
updateAllRooms function = do
   allRooms <- fetchRooms
   let updated = map function allRooms
   BL.writeFile roomsJSON $ encode updated
   return True

{-
   Funcao para deletar uma sala cadastrada.
-}
deleteRoom :: String -> IO Bool
deleteRoom codeStr = do
   allRooms <- fetchRooms
   let removed = filter (\room -> code room /= codeStr) allRooms
   if allRooms /= removed
      then do {BL.writeFile roomsJSON $ encode removed; return True}
      else return False

{-
   Funcao para obter uma sala cadastrada.
-}
getRoom :: String -> IO (Maybe Room)
getRoom codeStr = do
   allRooms <- fetchRooms
   let corresponding = filter (\room -> code room == codeStr) allRooms
   if null corresponding
      then return Nothing
      else return $ Just (head corresponding)

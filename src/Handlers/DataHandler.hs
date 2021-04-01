{-# LANGUAGE OverloadedStrings #-}
module Handlers.DataHandler where

import qualified Data.ByteString.Lazy as BL

import Manager

userJSON :: FilePath
userJSON = "data/userData.json"

instance FromJSON User where
instance ToJSON User where

instance FromJSON UserFull where
instance ToJSON UserFull where

noUsersYet :: IO Bool
noUsersYet = do
   (Just existingUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   return (null existingUsers)

saveUser :: UserFull -> IO Bool
saveUser newUser = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == email newUser) allUsers
   if null correspondingUsers
      then do {BL.writeFile userJSON $ encode (allUsers ++ [newUser]); return True}
      else return False

deleteUser :: String -> IO Bool
deleteUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let allButOne = filter (\user -> email user /= emailStr) allUsers
   BL.writeFile userJSON $ encode allButOne
   return $ not (allUsers == allButOne) -- If both arrays are equal, then nothing was removed

getUser :: String -> IO (Maybe UserFull)
getUser emailStr = do
   (Just allUsers) <- decode <$> BL.readFile userJSON :: IO (Maybe [UserFull])
   let correspondingUsers = filter (\user -> email user == emailStr) allUsers
   if null correspondingUsers
      then return Nothing
      else return $ Just (head correspondingUsers)



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

fetchRooms :: IO [Room]
fetchRooms = do
   (Just allRooms) <- decode <$> BL.readFile roomsJSON :: IO (Maybe [Room])
   return allRooms

noRoomsYet :: IO Bool
noRoomsYet = do null <$> fetchRooms

saveRoom :: Room -> IO Bool
saveRoom newRoom = do
   allRooms <- fetchRooms
   let correspondingRooms = filter (\room -> code room == code newRoom) allRooms
   if null correspondingRooms
      then do {BL.writeFile roomsJSON $ encode (allRooms ++ [newRoom]); return True}
      else return False

updateRoom :: String -> Room -> IO Bool
updateRoom codeRoom newRoom = do
   operate <- deleteRoom codeRoom
   saveRoom newRoom
      
updateAllRooms :: (Room -> Room) -> IO Bool
updateAllRooms function = do
   allRooms <- fetchRooms
   let updated = map function allRooms
   BL.writeFile roomsJSON $ encode updated
   return True

deleteRoom :: String -> IO Bool
deleteRoom codeStr = do
   allRooms <- fetchRooms
   let removed = filter (\room -> code room /= codeStr) allRooms
   if allRooms /= removed
      then do {BL.writeFile roomsJSON $ encode removed; return True}
      else return False

getRoom :: String -> IO (Maybe Room)
getRoom codeStr = do
   allRooms <- fetchRooms
   let corresponding = filter (\room -> code room == codeStr) allRooms
   if null corresponding
      then return Nothing
      else return $ Just (head corresponding)

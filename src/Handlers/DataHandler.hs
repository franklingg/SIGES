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

instance FromJSON Resource where
instance ToJSON Resource where

instance FromJSON RoomCategory where
instance ToJSON RoomCategory where

instance FromJSON Appointment where
instance ToJSON Appointment where

instance FromJSON Room where
instance ToJSON Room where


noRoomsYet :: IO Bool
noRoomsYet = do
   (Just existingRooms) <- decode <$> BL.readFile roomsJSON :: IO (Maybe [Room])
   return $ null existingRooms
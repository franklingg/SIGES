{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

import Data.ByteString
import Data.Text
import GHC.Generics

{-         USER HANDLER        -}
data User = User {
    nameUser :: String
   ,emailUser :: String
   ,isAdminUser :: Bool
} deriving(Eq, Generic)

data UserFull = UserFull {
    name :: String
   ,email :: String
   ,password :: Text
   ,registrationDate :: String
   ,isAdmin :: Bool
} deriving(Eq, Show, Generic)

{-         ERROR HANDLER         -}
type ErrorLog = String

{-         OUTPUT SCREENS        -}
data Screen = ExitScreen
            | FirstScreen
            | StartScreen 
            | LoginScreen 
            | LoggedScreen User
            | AdminScreen User
            | SignUpScreen User
            | RemoveUserScreen User
            deriving (Eq)

{-         ROOMS HANDLER         -}
data Room = Room {
    code :: String
   ,schedule :: [Appointment]
   ,resources :: [Resource]
   ,capacity :: Int
   ,localization :: String
   ,category :: RoomCategory

} deriving (Eq, Generic)

data Appointment = Appointment {
    requester :: User
   ,description :: String 
   ,time :: String

} deriving (Eq, Generic)

data RoomCategory = Laboratory
                  | Auditorium
                  | Classroom
                  | Office
                  | ConferenceRoom
                  deriving (Eq, Generic)

data Resource = StudentDesk Int
              | Projector Int
              | Microscope Int
              | Desk Int
              | Computador Int
              | Board Int
              | LabBench Int
              | AirConditioner Int
              deriving (Eq, Generic)

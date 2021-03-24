{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

import Data.ByteString
import Data.Time
import Data.Text
import Data.Dates
import GHC.Generics

{-         USER HANDLER        -}
data User = User {
    nameUser :: String
   ,emailUser :: String
   ,isAdminUser :: Bool
} deriving(Eq)

data UserFull = UserFull {
    name :: String
   ,email :: String
   ,password :: Text
   ,registrationDate :: UTCTime
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

} deriving (Eq)

data Appointment = Appointment {
    requester :: User
   ,description :: String 
   ,time :: DateTime

} deriving (Eq)

data RoomCategory = Laboratory
                  | Auditorium
                  | Classroom
                  | Office
                  | ConferenceRoom
                  deriving (Eq)

data Resource = StudentDesk Int
              | Projector Int
              | Microscope Int
              | Desk Int
              | Computador Int
              | Board Int
              | LabBench Int
              | AirConditioner Int
              deriving (Eq)

{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

import Data.ByteString
import Data.Text
import GHC.Generics
import Data.Time

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
   ,schedule :: [Reservation]
   ,resources :: [Resource]
   ,capacity :: Int
   ,localization :: String
   ,category :: RoomCategory

} deriving (Eq, Show, Generic)

data Reservation = Reservation {
    requester :: String
   ,description :: String 
   ,startTime :: LocalTime
   ,finishTime :: LocalTime

} deriving (Eq, Show, Generic)

data RoomCategory = Laboratory
                  | Auditorium
                  | Classroom
                  | Office
                  | ConferenceRoom
                  deriving (Eq, Show, Generic)

data ResourceKind = StudentDesk
              | Projector
              | Microscope
              | Desk
              | Computer
              | Board
              | LabBench
              | AirConditioner
              deriving (Eq, Show, Generic)

data Resource = Resource {
    resourceKind :: ResourceKind
   ,resourceQuantity :: Int 
}deriving (Eq, Show, Generic)


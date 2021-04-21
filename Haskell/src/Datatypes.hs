{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

-- importa ByteString
import Data.ByteString
-- importa Text
import Data.Text
-- importa Generics
import GHC.Generics
-- importa Time
import Data.Time

{-         Usuario Administrador        -}
data User = User {
    nameUser :: String
   ,emailUser :: String
   ,isAdminUser :: Bool
} deriving(Eq, Generic)

-- Dados do Administrador
data UserFull = UserFull {
    name :: String
   ,email :: String
   ,password :: Text
   ,registrationDate :: String
   ,isAdmin :: Bool
} deriving(Eq, Show, Generic)

{-         ERROR HANDLER         -}
type ErrorLog = String

-- Dados das Telas de saida
data Screen = ExitScreen
            | FirstScreen
            | StartScreen 
            | LoginScreen 
            | LoggedScreen
            | AdminScreen 
            | SignUpScreen
            | RemoveUserScreen
            deriving (Eq)

-- Dados das Salas
data Room = Room {
    code :: String
   ,schedule :: [Reservation]
   ,resources :: [Resource]
   ,capacity :: Int
   ,localization :: String
   ,category :: RoomCategory

} deriving (Eq, Generic)

-- Dados das reservas
data Reservation = Reservation {
    requester :: String
   ,description :: String 
   ,startTime :: LocalTime
   ,finishTime :: LocalTime

} deriving (Eq, Generic)

-- Dados das categorias das salas
data RoomCategory = Laboratory
                  | Auditorium
                  | Classroom
                  | Office
                  | ConferenceRoom
                  deriving (Eq, Generic)

-- Dados dos tipos de recursos
data ResourceKind = StudentDesk
              | Projector
              | Microscope
              | Desk
              | Computer
              | Board
              | LabBench
              | AirConditioner
              deriving (Eq, Show, Generic)

-- Dados dos recursos
data Resource = Resource {
    resourceKind :: ResourceKind
   ,resourceQuantity :: Int 
}deriving (Eq, Show, Generic)


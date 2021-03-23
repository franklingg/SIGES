{-# LANGUAGE DeriveGeneric #-}
module Datatypes where

import Data.ByteString
import Data.Time
import Data.Text
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
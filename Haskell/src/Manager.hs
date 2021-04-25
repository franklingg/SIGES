{-|
Module      : Manager
Description : Módulo que reúne várias operações de importação necessárias aos demais módulos do sistema para despoluir o código.
Copyright   : (c)
-}
module Manager(
    module Data.Maybe
   ,module Data.Char
   ,module Data.Either
   ,module Data.List
   ,module Data.List.Split
   ,module Data.Password.Validate
   ,module Data.Password.Bcrypt
   ,module Data.Aeson
   ,module System.IO
   ,module System.Exit
   ,module Text.Email.Validate
   ,module Datatypes
   ,module Data.Time
   ,module System.Console.ANSI
   ,module Control.Monad    
   ,module System.Directory
   ,module System.Environment
) where

import Data.Maybe
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Password.Validate (isValidPassword, defaultPasswordPolicy_)
import Data.Password.Bcrypt
import Data.Aeson
import System.IO
import System.Exit
import Text.Email.Validate
import Data.Time
import Datatypes
import System.Console.ANSI
import Control.Monad    
import System.Directory
import System.Environment
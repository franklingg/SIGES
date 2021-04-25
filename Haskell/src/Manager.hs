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
   ,module Data.Time
   ,module Data.List.Split
   ,module Data.Password.Validate
   ,module Data.Password.Bcrypt
   ,module Data.Aeson
   ,module Data.Functor
   ,module System.IO
   ,module System.Exit
   ,module System.Info
   ,module Text.Email.Validate
   ,module Datatypes
   ,module System.Console.ANSI
   ,module Control.Monad
   ,module Control.Monad.Loops
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
import Data.Time
import Data.Functor
import System.IO
import System.Exit
import System.Info
import Text.Email.Validate
import Datatypes
import System.Console.ANSI
import Control.Monad   
import Control.Monad.Loops 
import System.Directory
import System.Environment

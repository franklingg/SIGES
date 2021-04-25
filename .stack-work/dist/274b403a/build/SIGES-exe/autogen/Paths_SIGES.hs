{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SIGES (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\bin"
libdir     = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\lib\\x86_64-windows-ghc-8.10.4\\SIGES-0.1.0.0-4ks3lKD9s8TFFG7I5VPE6Q-SIGES-exe"
dynlibdir  = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\share\\x86_64-windows-ghc-8.10.4\\SIGES-0.1.0.0"
libexecdir = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\libexec\\x86_64-windows-ghc-8.10.4\\SIGES-0.1.0.0"
sysconfdir = "C:\\Users\\Deilton\\Documents\\Estudos\\PLP\\SIGES\\.stack-work\\install\\805ae311\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SIGES_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SIGES_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SIGES_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SIGES_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SIGES_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SIGES_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_roll_your_own (
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

bindir     = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/bin"
libdir     = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/lib/x86_64-linux-ghc-8.8.4/roll-your-own-0.1.0.0-ASLm7PcjcEIB1IA3EdHycn-roll-your-own"
dynlibdir  = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/share/x86_64-linux-ghc-8.8.4/roll-your-own-0.1.0.0"
libexecdir = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/libexec/x86_64-linux-ghc-8.8.4/roll-your-own-0.1.0.0"
sysconfdir = "/home/binso/projects/haskell/haskell-programming-from-first-principles-solutions/ch23/roll_your_own/roll-your-own/.stack-work/install/x86_64-linux-tinfo6/d72d56635939db0bdc2a2d6e10db4f568a5675c5d36035393c383c5d6239fba9/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "roll_your_own_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "roll_your_own_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "roll_your_own_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "roll_your_own_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "roll_your_own_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "roll_your_own_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

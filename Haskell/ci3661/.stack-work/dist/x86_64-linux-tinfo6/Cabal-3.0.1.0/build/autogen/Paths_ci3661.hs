{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ci3661 (
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

bindir     = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/bin"
libdir     = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/lib/x86_64-linux-ghc-8.8.4/ci3661-0.1.0.0-4kaEuo5A8D0DkuSeu3BBFp"
dynlibdir  = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/share/x86_64-linux-ghc-8.8.4/ci3661-0.1.0.0"
libexecdir = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/libexec/x86_64-linux-ghc-8.8.4/ci3661-0.1.0.0"
sysconfdir = "/home/jesus/Documents/Lenguajes de Programacion/Proyectos/Proyectos_Lenguajes_Lab/Haskell/ci3661/.stack-work/install/x86_64-linux-tinfo6/795e90bfacc5c6e0cc60d6a5758c23f0adb7ff42ecdfec24e672866f16761be7/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ci3661_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ci3661_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ci3661_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ci3661_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ci3661_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ci3661_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

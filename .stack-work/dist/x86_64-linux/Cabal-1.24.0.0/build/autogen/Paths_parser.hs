{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_parser (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lush/dev/parser/.stack-work/install/x86_64-linux/lts-7.2/8.0.1/bin"
libdir     = "/home/lush/dev/parser/.stack-work/install/x86_64-linux/lts-7.2/8.0.1/lib/x86_64-linux-ghc-8.0.1/parser-0.1"
datadir    = "/home/lush/dev/parser/.stack-work/install/x86_64-linux/lts-7.2/8.0.1/share/x86_64-linux-ghc-8.0.1/parser-0.1"
libexecdir = "/home/lush/dev/parser/.stack-work/install/x86_64-linux/lts-7.2/8.0.1/libexec"
sysconfdir = "/home/lush/dev/parser/.stack-work/install/x86_64-linux/lts-7.2/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

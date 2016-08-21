module Paths_parser (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/tim/devel/haskell/parser/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/bin"
libdir     = "/home/tim/devel/haskell/parser/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/lib/x86_64-linux-ghc-7.10.3/parser-0.1-3QpcwcPCOtoLM4yLfMygQp"
datadir    = "/home/tim/devel/haskell/parser/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/share/x86_64-linux-ghc-7.10.3/parser-0.1"
libexecdir = "/home/tim/devel/haskell/parser/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/libexec"
sysconfdir = "/home/tim/devel/haskell/parser/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/etc"

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

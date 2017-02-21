{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_httpsproxy (
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

bindir     = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/bin"
libdir     = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/lib/x86_64-linux-ghc-8.0.2/httpsproxy-0.1.0.0-LQuZUq0pf234VyKYYYfUqC"
dynlibdir  = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/share/x86_64-linux-ghc-8.0.2/httpsproxy-0.1.0.0"
libexecdir = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/libexec"
sysconfdir = "/home/paddy/Desktop/httpsproxy/.stack-work/install/x86_64-linux/nightly-2017-02-20/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "httpsproxy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "httpsproxy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "httpsproxy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "httpsproxy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "httpsproxy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "httpsproxy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

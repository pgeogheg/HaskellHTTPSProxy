#!/usr/bin/env runghc

{-# LANGUAGE TupleSections #-}

import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..), toFlag)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Verbosity (silent)
import System.Info (os)
import qualified Control.Exception as E (tryJust, throw)
import System.IO.Error (isUserError)
import Control.Monad (forM)
import Data.List

-- On macOS we're checking whether OpenSSL library is avaiable
-- and if not, we're trying to find Homebrew or MacPorts OpenSSL installations.
--
-- Method is dumb -- set homebrew-openssl or macports-openssl flag and try
-- to configure and check C libs.
--
-- If no or multiple libraries are found we display error message
-- with instructions.

main
    | os == "darwin" =
        defaultMainWithHooks simpleUserHooks { confHook = conf }
    | otherwise =
        defaultMain

flags = ["homebrew-openssl", "macports-openssl"]

conf descr cfg = do
    c <- tryConfig descr cfg
    case c of
        Right lbi -> return lbi -- library was found
        Left e
            | configConfigurationsFlags cfg
              `intersect` [(FlagName f, True) | f <- flags] /= [] ->
                E.throw e
                -- flag was set but library still wasn't found
            | otherwise -> do
                r <- forM flags $ \ f ->
                    fmap (f,) $ tryConfig descr $
                    setFlag f cfg { configVerbosity = toFlag silent }
                    -- TODO: configure is a long operation
                    -- while checkForeignDeps is fast.
                    -- Perhaps there is a way to configure once
                    -- and only apply flags to result and check.
                    -- However, additional `configure`s happen only on macOS
                    -- and only when library wasn't found.
                case [(f,r) | (f, Right r) <- r] of
                    [(_,lbi)] ->
                        return lbi -- library was found
                    [] ->
                        fail notFound
                    fs ->
                        fail $ multipleFound fs

notFound =
    "Can't find OpenSSL library,\n\
    \install it via 'brew install openssl' or 'port install openssl'\n\
    \or use --extra-include-dirs= and --extra-lib-dirs=\n\
    \to specify location of installed OpenSSL library."

multipleFound fs =
    "Multiple OpenSSL libraries were found,\n\
    \use " ++ intercalate " or " ["'-f " ++ f ++ "'" | (f,_) <- fs] ++ "\n\
    \to specify location of installed OpenSSL library."

setFlag f c = c { configConfigurationsFlags = go (configConfigurationsFlags c) }
    where go [] = []
          go (x@(FlagName n, _):xs)
              | n == f = (FlagName f, True) : xs
              | otherwise = x : go xs

tryConfig descr flags = do
    lbi <- confHook simpleUserHooks descr flags
    -- confHook simpleUserHooks == Distribution.Simple.Configure.configure

    -- Testing whether C lib and header dependencies are working.
    -- We check exceptions only here, to check C libs errors but not other
    -- configuration problems like not resolved .cabal dependencies.
    E.tryJust ue $ do
        postConf simpleUserHooks [] flags (localPkgDescr lbi) lbi
        -- postConf simpleUserHooks ~==
        --   Distribution.Simple.Configure.checkForeignDeps

        return lbi

    where ue e | isUserError e = Just e
               | otherwise = Nothing

module Main (main) where

import OpenSSL.RSA
import OpenSSL.DER
import Test.OpenSSL.TestUtils

main :: IO ()
main = do
    keyPair <- generateRSAKey 1024 3 Nothing
    pubKey <- rsaCopyPublic keyPair
    assertEqual "encodeDecode" (Just pubKey) (fromDERPub (toDERPub keyPair))

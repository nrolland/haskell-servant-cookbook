{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                   (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant
import           Servant.API
import           SimpleApi

 --  openssl genrsa -des3 -out server.key 1024
 --  openssl req -new -key server.key -out server.csr
 --  mv server.key server.key.org
 --  openssl rsa -in server.key.org -out server.key
 --  openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

main :: IO ()
main = do
    runTLS (tlsSettings "server.crt" "server.key") (setPort 8080 defaultSettings) app

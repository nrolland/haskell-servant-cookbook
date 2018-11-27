{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Network.HTTP.Client.TLS    (tlsManagerSettings, mkManagerSettings)
import           Servant.Client
import           SimpleApi
import qualified Network.Connection as NC (TLSSettings(..))

import           Network.HTTP.Client        (newManager)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    stgs <- do
      choice <- do
        T.putStrLn "1. try with normal certificate check \n2. try with no check"
        T.getLine
      return $ case choice of
        "2" ->  (mkManagerSettings (NC.TLSSettingsSimple True False False) Nothing)
        "1" ->  tlsManagerSettings

    manager <- newManager stgs
    let baseUrl = BaseUrl Https "localhost" 8080 ""
    res <- queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p

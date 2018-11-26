{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Protolude hiding (packageName)
import Prelude ()
import Data.Aeson
import Data.Text (Text)
import Servant.API
import Servant.Client
import Network.HTTP.Client (defaultManagerSettings, newManager, proxyHost, proxyPort,useProxy,managerSetProxy)
import qualified  Network.HTTP.Client as H( Proxy(..))

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- hackage.haskell.org is configured incomaptibly with servant, which adds an extra charset in the Accept header
-- 'Accept:application/json;charset=utf-8'
-- This seem to be a mess on the interweb https://github.com/request/request/issues/383 impacting Wai and servant
-- It can be seen by using
-- export http_proxy="http://localhost:8080"
-- and mitmproxy to see intercepted requests made by `stack exec hackage-api`

type HackageAPI =
       "users" :> Get '[JSON] [UserSummary]
  :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]


type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username"
                <*> o .: "userid"
  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON Package


hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: ClientM [UserSummary]
getUser :: Username -> ClientM UserDetailed
getPackages :: ClientM [Package]
getUsers :<|> getUser :<|> getPackages = client hackageAPI

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO ()
uselessNumbers = do
  res <- getUsers'
  case res of Left e ->  T.putStrLn $ "error retrieving users"  <> (e &show &T.pack)
              Right users -> putStrLn $ show (length users) ++ " users"
  user <- do
     T.putStrLn "Enter a valid hackage username"
     T.getLine
  res <- getUser' user
  case res of Left e -> T.putStrLn $ "error retrieving info for user " <> (user &show &T.pack)  <> (e &show &T.pack)
              Right userDetail -> T.putStrLn $ user <> " maintains " <> (groups userDetail &length &show &T.pack) <> " packages"

  res <- getPackages'
  case res of
    Left e ->  T.putStrLn $ "error retrieving packages" <> (e &show &T.pack)
    Right packages -> do let monadPackages = filter (isMonadPackage . packageName) packages
                         putStrLn $ show (length monadPackages) ++ " monad packages"

  where
      isMonadPackage = T.isInfixOf "monad"
      changeproxy = H.Proxy { proxyHost="localhost", proxyPort=8080} & useProxy & managerSetProxy
      imanager = defaultManagerSettings & newManager
      url = BaseUrl Http "hackage.haskell.org" 80 ""
      clientOp op = imanager >>= \manager -> runClientM op $ mkClientEnv manager url
      getUser' u = clientOp  (getUser u)
      getUsers' =  clientOp  getUsers
      getPackages' = clientOp getPackages

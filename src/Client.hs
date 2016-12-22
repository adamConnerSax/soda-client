{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Client where

import Types

import Servant.API
import Servant.Client
import Servant.Common.Req

import Web.HttpApiData (ToHttpApiData(..))
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Monoid ((<>))

import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except (runExceptT)



import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol,KnownSymbol)



addFilter::SocrataFilter->SocrataFilters->SocrataFilters
addFilter (SocrataFilter c v) qm = M.insert c v qm

newtype SocrataResult = SocrataResult (V.Vector (HM.HashMap T.Text T.Text)) deriving (Generic,Show)
instance FromJSON SocrataResult

type SoQL = T.Text

data MultiQueryParams (sym :: Symbol) a
    deriving Typeable

instance (KnownSymbol sym, ToHttpApiData a, HasClient api)
      => HasClient (MultiQueryParams sym a :> api) where

  type Client (MultiQueryParams sym a :> api) =
    QueryMap a -> Client api

  clientWithRoute Proxy req queryMap =
    clientWithRoute (Proxy :: Proxy api)
                    (foldl' (\ req' (qn,mqv) -> appendToQueryString qn mqv req')
                            req
                            queryList
                    )

    where queryList = M.toList ((Just . toQueryParam) <$> queryMap)


type SodaAPI = "resource" :> Capture "datasetID" DatasetIdentifier :> MultiQueryParams "filter" SocrataValue :> Get '[JSON] SocrataResult :<|>
               "resource" :> Capture "datasetID" DatasetIdentifier :> QueryParam "$query" SoQL :> Get '[JSON] SocrataResult

sodaApi::Proxy SodaAPI
sodaApi = Proxy

filterReq :<|> soqlReq = client sodaApi

testQuery :: Manager -> ClientM SocrataResult
testQuery mgr = do
  let filterZip = SocrataFilter "zip_code" "11215"
      filterStreet = SocrataFilter "on_street_name" "PRESIDENT STREET"
      filters = addFilter filterZip . addFilter filterStreet $ M.empty
      baseUrl = BaseUrl Http "data.cityofnewyork.us" 80 ""
  filterReq (DatasetIdentifier "qiz3axqb") filters mgr baseUrl

testSoQL :: Manager -> ClientM SocrataResult
testSoQL mgr = do
  let sql = "SELECT date,number_of_pedestrians_killed WHERE number_of_pedestrians_killed > 1 "
      baseUrl = BaseUrl Http "data.cityofnewyork.us" 80 ""
  soqlReq (DatasetIdentifier "qiz3axqb") (Just sql) mgr baseUrl

testRun:: IO ()
testRun = do
  mgr <- newManager defaultManagerSettings
  result <- runExceptT $ testSoQL mgr
  case result of
    Left err -> putStrLn $ "Error: " ++ (show err)
    Right (SocrataResult t) -> putStrLn $ "Result: \n" ++ (show t)

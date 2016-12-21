{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module SODA where
import Servant.API
import Servant.Client
import Servant.Common.Req
import Web.HttpApiData (ToHttpApiData(..))
import Network.HTTP.Client (newManager, defaultManagerSettings,Manager)
import GHC.Generics
import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Char (isLower,isDigit)
import Data.List (foldl')
import Control.Monad.Except (runExceptT)

import           Data.Typeable (Typeable)
import           GHC.TypeLits (Symbol,KnownSymbol)

data DatasetIdentifier = DatasetIdentifier T.Text
instance ToHttpApiData DatasetIdentifier where
  toUrlPiece (DatasetIdentifier di) = let (a,b) = T.splitAt 4 di in a <> "-" <> b <> ".json"

validateIdentifier::DatasetIdentifier->Maybe DatasetIdentifier
validateIdentifier (DatasetIdentifier di) = if (rightLength di && rightChars di) then Just (DatasetIdentifier di) else Nothing where
  rightLength t = T.length t == 8
  rightChar c = isLower c || isDigit c
  rightChars t = T.foldl (\b c-> b && (rightChar c)) True t

type SocrataColumnName = T.Text
type SocrataValue = T.Text -- how do we type this

data SocrataFilter  = SocrataFilter { column::SocrataColumnName, value::SocrataValue }

newtype SocrataFilterSet = SocrataFilterSet [SocrataFilter]
--instance ToHttpApiData SocrataFilterSet where
--  toUrlPiece (SocrataFilterSet fs) = (T.intercalate "&" $ ((\(SocrataFilter c v)->c <> "=" <> v) <$> fs))

toMultiQuery::SocrataFilterSet -> [(T.Text,SocrataValue)]
toMultiQuery (SocrataFilterSet fs) = map (\(SocrataFilter c v) -> (c,v)) fs

newtype SocrataResult = SocrataResult T.Text deriving (Show,Generic)
instance FromJSON SocrataResult

data MultiQueryParams (sym :: Symbol) a
    deriving Typeable


instance (KnownSymbol sym, ToHttpApiData a, HasClient api)
      => HasClient (MultiQueryParams sym a :> api) where

  type Client (MultiQueryParams sym a :> api) =
    [(T.Text,a)] -> Client api

  clientWithRoute Proxy req queryList = -- queryList is [(T.Text,a)]
    clientWithRoute (Proxy :: Proxy api)
                    (foldl' (\ req' (qn,mqv) -> appendToQueryString qn mqv req')
                            req
                            queryList'
                    )

    where queryList' = map (\(x,y)->(x,Just $ toQueryParam y)) queryList


type SodaAPI = "resource" :> Capture "datasetID" DatasetIdentifier :> MultiQueryParams "filter" SocrataValue :> Get '[JSON] SocrataResult
sodaApi::Proxy SodaAPI
sodaApi = Proxy

filterReq = client sodaApi

testQuery :: Manager->ClientM SocrataResult
testQuery mgr = do
  let filterZip = SocrataFilter "zip_code" "11215"
      filterStreet = SocrataFilter "on_street_name" "PRESIDENT STREET"
      filters = SocrataFilterSet [filterZip,filterStreet]
      baseUrl = BaseUrl Http "data.cityofnewyork.us" 80 ""
  filterReq (DatasetIdentifier "qiz3axqb") (toMultiQuery filters) mgr baseUrl

testRun:: IO ()
testRun = do
  mgr <- newManager defaultManagerSettings
  result <- runExceptT $ testQuery mgr
  case result of
    Left err -> putStrLn $ "Error: " ++ (show err)
    Right (SocrataResult t) -> putStrLn . T.unpack $ "Result: \n" <> t

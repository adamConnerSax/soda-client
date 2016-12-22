{-# LANGUAGE OverloadedStrings #-}
module Types where

import Web.HttpApiData (ToHttpApiData(..))
import qualified Data.Text as T
import Data.Char (isLower,isDigit)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))

data DatasetIdentifier = DatasetIdentifier T.Text
instance ToHttpApiData DatasetIdentifier where
  toUrlPiece (DatasetIdentifier di) = let (a,b) = T.splitAt 4 di in a <> "-" <> b <> ".json"

validateIdentifier::DatasetIdentifier->Maybe DatasetIdentifier
validateIdentifier (DatasetIdentifier di) = if (rightLength di && rightChars di) then Just (DatasetIdentifier di) else Nothing where
  rightLength t = T.length t == 8
  rightChar c = isLower c || isDigit c
  rightChars t = T.foldl (\b c-> b && (rightChar c)) True t

type QueryMap a = M.Map T.Text a

type SocrataColumnName = T.Text
type SocrataValue = T.Text -- how do we type this

data SocrataFilter  = SocrataFilter { column::SocrataColumnName, value::SocrataValue }

type SocrataFilters = QueryMap SocrataValue

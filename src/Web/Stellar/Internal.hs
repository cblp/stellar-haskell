{-
 - todo bobjflong: move this to Types.hs
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Internal where

import           Control.Monad
import           Data.Aeson
import           Data.Text

data APIMoney = ExtractedText {
  innerMoney :: Text
} deriving (Show, Eq)

emptyAPIMoney :: APIMoney
emptyAPIMoney = ExtractedText mempty

instance FromJSON APIMoney where
  parseJSON (Object o) = ExtractedText <$> (o .: "value")
  parseJSON (String s) = return $ ExtractedText s
  parseJSON _ = mzero

data APICurrency = ExtractedCurrency {
  innerCurrency :: Text
} deriving (Show, Eq)

defaultAPICurrency :: APICurrency
defaultAPICurrency = ExtractedCurrency mempty

instance FromJSON APICurrency where
  parseJSON (Object o) = ExtractedCurrency <$> (o .: "currency")
  parseJSON _ = return defaultAPICurrency

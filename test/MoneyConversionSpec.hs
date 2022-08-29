{-# LANGUAGE OverloadedStrings #-}

module MoneyConversionSpec (spec) where

import           Control.Lens            hiding (elements)
import           Test.Hspec
import           Test.QuickCheck
import           Web.Stellar.AccountLine

spec :: Spec
spec = do
  describe "MoneyLens laws" $ do
    it "sets what it gets" $
      property $ \v -> v == (balance .~ (v ^. balance) $ v)
    it "gets back what it puts in" $
      property $ \m a -> Just m == (balance .~ (Just m) $ a) ^. balance
    it "sets twice safely" $
      property $ \m a ->
                  (balance .~ (Just m) $ a) == (balance .~ (Just m) $ (balance .~ (Just m) $ a))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Account (
    fetchAccount,
    MicroStellar(..),
    Account(..),
    flags,
    ownerCount,
    previousTransactionID,
    previousTxnLgrSeq,
    stellarSequence,
    stellarIndex,
    balance
  ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Data.Aeson
import           Data.Text
import           Lens.Family
import           Web.Stellar.Request

-- | Provides a lens interface onto a Stellar account
--
-- >>> account ^. balance
-- MicroStellar 6900888889
--
-- >>> account ^. flags
-- 1048576
--
-- >>> account ^. ownerCount
-- 0
--
-- >>> account ^. previousTransactionID
-- "A584CEF24F24DAA16E99C2858B62A978975C448181FBF34B21B5873AFBA6A8AB
--
-- >>> account ^. previousTxnLgrSeq
-- 1056876
--
-- >>> account ^. stellarSequence
-- 4229
--
-- >>> account ^. stellarIndex
-- "6047FB9C7976F2D0554618F5ABFF423E7136205BAF19E92BE9D295E549442C45"
data Account = Account {
  _balanceData           :: Text,
  _flags                 :: Int,
  _ownerCount            :: Int,
  _previousTransactionID :: Text,
  _previousTxnLgrSeq     :: Int,
  _stellarSequence       :: Int,
  _stellarIndex          :: Text
} deriving (Show)

$(makeLenses ''Account)

-- MicroStellars are the smallest divisible Stellar Unit
data MicroStellar = MicroStellar Int | Unparseable deriving (Show)

balance :: Lens' Account MicroStellar
balance = lens getBalance setBalance
  where getBalance acc = case (reads (unpack $ _balanceData acc) :: [(Int, String)]) of
                           [(a,"")] -> MicroStellar a
                           _ -> Unparseable
        setBalance a (MicroStellar x) = a { _balanceData = (pack $ show x) }
        setBalance a _ = a { _balanceData = "unknown" }

instance FromJSON Account where
  parseJSON (Object v) = do
    Account <$> v .: "Balance"
    <*> v .: "Flags"
    <*> v .: "OwnerCount"
    <*> v .: "PreviousTxnID"
    <*> v .: "PreviousTxnLgrSeq"
    <*> v .: "Sequence"
    <*> v .: "index"

data AccountData = AccountData {
  innerAccount :: Account
} deriving (Show)

instance FromJSON AccountData where
  parseJSON (Object v) = do
    AccountData <$> ((v .: "result") >>= (.: "account_data"))

data AccountInfoRequest = AccountInfoRequest {
  account :: Text
}

instance ToJSON AccountInfoRequest where
  toJSON accountInfoRequest = object [
    "method" .= ("account_info" :: Text),
    "params" .= [object ["account" .= (account accountInfoRequest)]]]

-- | Fetch account information for an Account ID
--
-- >>> r <- fetchAccount "https://test.stellar.org:9002" "abcdef..."
fetchAccount :: StellarEndpoint -> Text -> IO (Maybe Account)
fetchAccount endpoint accountId = do
  accountData <- fetchTestAccountData
  return $ fmap innerAccount accountData
  where fetchTestAccountData :: IO (Maybe AccountData)
        fetchTestAccountData = fmap decode $ makeRequest endpoint $ AccountInfoRequest accountId
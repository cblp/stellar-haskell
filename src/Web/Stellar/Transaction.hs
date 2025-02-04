{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Stellar.Transaction (
    fetchTransactions,
    fetchTransactionsWithMarker,
    Transaction(..),
    earliestLedger,
    currentLedger,
    transactionAccount,
    amount,
    destination,
    signingPubKey,
    transactionType,
    TransactionType(..),
    transactionSignature,
    date,
    hash,
    currency,
    rawTransaction
  ) where

import           Control.Lens         hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Text
import qualified Data.Text            as T
import           Web.Stellar.Internal
import           Web.Stellar.Request

data Transaction = Transaction {
  _transactionAccount   :: Text,
  _destination          :: Maybe Text,
  _signingPubKey        :: Text,
  _transactionTypeData  :: Text,
  _transactionSignature :: Text,
  _date                 :: !Int,
  _hash                 :: Text,
  _amountData           :: Text,
  _currencyData         :: Text,
  _rawTransaction       :: ByteString
} deriving (Eq, Show)

data TransactionType =
  Payment
  | AccountSet
  | AccountMerge
  | OfferCreate
  | OfferCancel
  | TrustSet
  | Inflation
  | SetRegularKey
  | UnknownTransactionType
  deriving (Show, Read)

type TransactionTypeRead = [(TransactionType, String)]

$(makeLenses ''Transaction)

transactionType :: Lens' Transaction TransactionType
transactionType = lens g s
  where g v = case ((reads $ T.unpack $ v ^. transactionTypeData) :: TransactionTypeRead) of
                [(val, "")] -> val
                _ -> UnknownTransactionType
        s v t = transactionTypeData .~ (T.pack.show $ t) $ v

amount :: Lens' Transaction (Maybe Money)
amount = moneyLens amountData

currency :: Applicative f => (Text -> f Text) -> Transaction -> f Transaction
currency f t = case rawCurrency of
  "" -> pure t
  _ -> fmap (\x' -> currencyData .~ x' $ t) (f rawCurrency)
  where rawCurrency = t ^. currencyData

instance FromJSON Transaction where
  parseJSON (Object v) = do
    Transaction <$> (tx >>= (.: "Account"))
    <*> (tx >>= (.:? "Destination"))
    <*> (tx >>= (.: "SigningPubKey"))
    <*> (tx >>= (.: "TransactionType"))
    <*> (tx >>= (.: "TxnSignature"))
    <*> (tx >>= (.: "date"))
    <*> (tx >>= (.: "hash"))
    <*> fmap (extract innerMoney) (withDefault "Amount" emptyAPIMoney)
    <*> fmap (extract innerCurrency) (withDefault "Amount" defaultAPICurrency)
    <*> (tx >>= (\t -> return $ encode t))
    where extract _ Nothing = mempty
          extract f (Just x) = f x
          withDefault k d = (tx >>= (\o -> o .:? k .!= (Just d)))
          tx = (v .: "tx")
  parseJSON _ = mzero

data TransactionList = TransactionList {
  innerTransactions :: [Transaction]
}

instance FromJSON TransactionList where
  parseJSON (Object v) = do
    TransactionList <$> ((v .: "result") >>= (.: "transactions"))
  parseJSON _ = mzero

data TransactionRequest = TransactionRequest {
  account        :: Text,
  ledgerIndexMin :: Int,
  ledgerIndexMax :: Int,
  marker         :: Text
}

instance ToJSON TransactionRequest where
  toJSON transactionRequest = object [
    "method" .= ("account_tx" :: Text),
    "params" .= [object [
      "account" .= (account transactionRequest),
      "ledger_index_min" .= (ledgerIndexMin transactionRequest),
      "ledger_index_max" .= (ledgerIndexMax transactionRequest),
      "marker" .= (marker transactionRequest)]]]

type LedgerLimit = Int

earliestLedger :: LedgerLimit
earliestLedger = 0

currentLedger :: LedgerLimit
currentLedger = (-1)

fetchTransactions :: StellarEndpoint -> AccountID -> LedgerLimit -> LedgerLimit -> IO (Maybe [Transaction])
fetchTransactions endpoint acc fetchMin fetchMax = fetchTransactionsWithMarker endpoint acc fetchMin fetchMax ""

fetchTransactionsWithMarker :: StellarEndpoint -> AccountID -> LedgerLimit -> LedgerLimit -> Text -> IO (Maybe [Transaction])
fetchTransactionsWithMarker endpoint (AccountID acc) fetchMin fetchMax fetchMarker = do
  r <- fetchTransactionData
  return $ fmap innerTransactions r
  where fetchTransactionData = fmap decode rawRequestData
        rawRequestData = makeRequest endpoint $ TransactionRequest acc fetchMin fetchMax fetchMarker



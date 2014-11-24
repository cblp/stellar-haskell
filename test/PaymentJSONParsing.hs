{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PaymentJSONTests where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Fixed
import           Data.Maybe
import           Data.Text
import           Test.HUnit
import           Web.Stellar.Payment

payment1Encodes = TestCase (
                    assertEqual
                    "Basic payment params encoding"
                    res
                    (encode paymentParams1)
                  )
  where res = "{\"params\":[{\"secret\":\"2secret\",\"tx_json\":{\"TransactionType\":\"Payment\",\"Amount\":{\"value\":\"1\",\"currency\":\"USD\",\"issuer\":\"abc\"},\"Destination\":\"luckyduck\",\"Account\":\"moneybags\"}}],\"method\":\"submit\"}"
        amount1 = WithCurrency "USD" "abc" 1
        paymentParams1 = defaultPaymentParams & paymentAmount .~ amount1 &
                                                secret .~ "2secret" &
                                                fromAccount .~ "moneybags" &
                                                toAccount .~ "luckyduck"

result1Decodes = TestCase (
                   assertEqual
                   "Successful payment decoding"
                   PaymentSuccess
                   ((fromJust ((decode result1) :: Maybe PaymentResponse)) ^. status)
                 )
  where result1 = "{   \"result\" : {      \"engine_result\" : \"tesSUCCESS\",      \"engine_result_code\" : 0,      \"engine_result_message\" : \"The transaction was applied.\",      \"status\" : \"success\",      \"tx_blob\" : \"abc\",      \"tx_json\" : {         \"Account\" : \"abc\",         \"Amount\" : {            \"currency\" : \"USD\",            \"issuer\" : \"abc\",            \"value\" : \"1\"         },         \"Destination\" : \"abc\",         \"Fee\" : \"10\",         \"Flags\" : 2147483648,         \"Sequence\" : 2967,         \"SigningPubKey\" : \"abc\",         \"TransactionType\" : \"Payment\",         \"TxnSignature\" : \"abc\",         \"hash\" : \"abc\"      }   }}\r"

tests = TestList [
          TestLabel "#1 encodes" payment1Encodes
          , TestLabel "#1 decodes" result1Decodes
        ]

main = runTestTT tests
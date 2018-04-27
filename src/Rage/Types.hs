{-# LANGUAGE OverloadedStrings #-}
module Rage.Types where

import qualified Data.Text as T
import           Data.Time
import           Data.Word


newtype TxId = TxId T.Text
newtype Address = Address T.Text

data Locality = Local
              | Foreign

data Direction = Incoming
               | Outgoing

data TxMeta = TxMeta {
      txId      :: TxId
    , txInputs  :: [Address]
    , txOutputs :: [Address]
    , locality  :: Locality
    , direction :: Direction
    , createdAt :: UTCTime
}

data DB = DB {
    metas :: [TxMeta]
}

-- Generates some "big data(TM).
bigData :: DB
bigData = DB (replicate 1000000 sampleMeta)

sampleMeta = TxMeta {
    txId = TxId "abcdefg"
  , txInputs  = replicate 30 sampleInput
  , txOutputs = replicate 30 sampleOutput
  , locality = Local
  , direction = Outgoing
  , createdAt = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 10)
}

sampleInput  = Address "123"
sampleOutput = Address "abc"

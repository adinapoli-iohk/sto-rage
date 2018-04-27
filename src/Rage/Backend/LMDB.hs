{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rage.Backend.LMDB where

import qualified Data.Text            as T
import           Data.Time
import           Data.Word

import           Codec.Serialise
import           Database.LMDB.Simple
import           GHC.Generics

newtype TxId = TxId T.Text deriving (Show, Generic)
newtype Address = Address T.Text deriving (Show, Generic)

instance Serialise TxId
instance Serialise Address

data Locality = Local
              | Foreign
              deriving (Generic, Show)

instance Serialise Locality

data Direction = Incoming
               | Outgoing
               deriving (Generic, Show)

instance Serialise Direction

data TxMeta = TxMeta {
      txInputs  :: [Address]
    , txOutputs :: [Address]
    , locality  :: Locality
    , direction :: Direction
    , createdAt :: UTCTime
} deriving (Generic, Show)

instance Serialise TxMeta

data Storage e = Storage {
      env     :: (Environment e)
    , dbMetas :: Database TxId TxMeta
}

demo :: IO ()
demo = do
    env <- openReadWriteEnvironment "lmdb.dat" defaultLimits { maxDatabases = 1 }
    dbMetas <- readOnlyTransaction env $ getDatabase Nothing
    let storage = Storage env dbMetas
    x0 <- putTx storage (TxId "123") sampleMeta
    print x0
    x1 <- getTx (storage { env = readOnlyEnvironment env }) (TxId "123")
    print x1

putTx :: Storage ReadWrite -> TxId -> TxMeta -> IO ()
putTx Storage{..} tid tmeta = do
  readWriteTransaction env (put dbMetas tid (Just tmeta))

getTx :: Storage ReadOnly -> TxId -> IO (Maybe TxMeta)
getTx Storage{..} tid = do
  readOnlyTransaction env (get dbMetas tid)

sampleMeta = TxMeta {
    txInputs  = replicate 30 sampleInput
  , txOutputs = replicate 30 sampleOutput
  , locality = Local
  , direction = Outgoing
  , createdAt = UTCTime (ModifiedJulianDay 10) (secondsToDiffTime 10)
}

sampleInput  = Address "123"
sampleOutput = Address "abc"

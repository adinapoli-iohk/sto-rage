{-# LANGUAGE ScopedTypeVariables #-}
module Rage.Backend.Types where

import           Data.String.Conv

data Backend = AcidState
             | LMDB
             | TCache
             | VCache
             deriving (Show, Read, Eq, Enum, Bounded)


allBackends :: [String]
allBackends = map show ([minBound .. maxBound] :: [Backend])

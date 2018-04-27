{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.List          as List
import           Data.Monoid
import qualified Rage.Backend.LMDB  as LMDB
import           Rage.Backend.Types
import           Rage.Types
import           System.Environment
import           Text.Read

main :: IO ()
main = do
    a <- getArgs
    case a of
         [b] -> case List.find (== b) allBackends >>= readMaybe of
                     Nothing -> error $ "Usage: rage [backend], backend one of " <> show allBackends
                     Just LMDB -> LMDB.demo
                     Just (bEnd :: Backend) -> print bEnd

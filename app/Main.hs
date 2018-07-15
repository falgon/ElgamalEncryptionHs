{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    p <- genRndSafePrime 16
    (pubkey, prikey) <- genKeys p $ rootFromSafePrime p
    print . decode (pubkey, prikey) =<< encode pubkey . head =<< getArgs

{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
import System.Environment (getArgs)
import Control.Monad ((<=<))

main :: IO ()
main = do
    p <- genRndSafePrime 16
    (pubkey, prikey) <- genKeys p $ rootFromSafePrime p
    mapM_ (print . decode (pubkey, prikey) <=< encode pubkey) =<< getArgs

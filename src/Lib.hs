{-# OPTIONS_GHC -Wall #-}
module Lib ( 
    genKeys,
    genRndPrime,
    genRndSafePrime,
    rootFromSafePrime,
    encode,
    decode
) where

import Data.Tuple.Extra (first, second, dupe)
import Data.Bool (bool)
import Data.Bits (Bits, (.&.), (.|.), shiftR)
import Data.Char (ord, chr)
import Control.Monad.Fix (fix)
import Control.Monad (mapM)
import System.Random (Random, randomRs, newStdGen, randomRIO)
import System.IO.Unsafe (unsafePerformIO)

type BitSize = Int
type PublicKey = (Integer, Integer, Integer)
type PrivateKey = Integer
type Keys = (PublicKey, PrivateKey)
type Cryptogram = [Integer]

modExp :: (Integral a, Bits a) => a -> a -> a -> a
modExp = go 1
    where
        go r _ 0 _ = r
        go r x n m
            | n .&. 1 == 1 = go (r * x `rem` m) (x * x `rem` m) (n `shiftR` 1) m
            | otherwise = go r (x * x `rem` m) (n `shiftR` 1) m

gcdExt :: Integral a => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        (s, t, g) = gcdExt b r

modInv :: Integral a => a -> a -> Maybe a
modInv a m = case gcdExt a m of
    (x, _, 1) -> Just $ if x < 0 then x + m else x
    _ -> Nothing

{-# INLINE witnesses #-}
witnesses :: (Num a, Enum a, Ord a, Random a) => Int -> a -> IO [a]
witnesses t n 
    | n < 2047 = return [2]
    | n < 1373653 = return [2, 3]
    | n < 9080191 = return [31, 73]
    | n < 25326001 = return [2, 3, 5]
    | n < 3215031751 = return [2, 3, 5, 7]
    | n < 4759123141 = return [2, 7, 61]
    | n < 1122004669633 = return [2, 13, 23, 1662803]
    | n < 2152302898747 = return [2, 3, 5, 7, 11]
    | n < 3474749660383 = return [2, 3, 5, 7, 11, 13]
    | n < 341550071728321 = return [2, 3, 5, 7, 11, 13, 17]
    | n < 3825123056546413051 = return [2, 3, 5, 7, 11, 13, 17, 19, 23]
    | n < 18446744073709551616 = return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
    | n < 318665857834031151167461 = return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
    | n < 3317044064679887385961981 = return [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
    | otherwise = take t . randomRs (2, pred n) <$> newStdGen

millerRabin :: Int -> Integer -> IO Bool
millerRabin _ 0 = return False
millerRabin _ 1 = return False
millerRabin t n
    | even n = return $ n == 2
    | otherwise = all (\a -> (uncurry (||) . first (==1) . second (==pred n) . dupe . flip (`modExp` q) n) a || 
        any ((==pred n) . flip (modExp a) n . (*q) . (2^)) [0..pred k]) <$> witnesses t n
    where 
        (k, q) = first (pred . length) . second (fst . last) . dupe $ 
            takeWhile ((== 0). snd) $ iterate ((`quotRem` 2) . fst) (pred n, 0) 

{-# INLINE millerRabin' #-}
millerRabin' :: Int -> Integer -> Bool
millerRabin' t n = unsafePerformIO $ millerRabin t n

{-# INLINE genRndPrime #-}
genRndPrime :: BitSize -> IO Integer
genRndPrime b = let t | b > 3071 = 64 | b > 2047 = 56 | otherwise = 40 in
    fix $ \loop -> uncurry (bool loop) . first return . second (millerRabin' t) . dupe =<< (.|. 1) <$> randomRIO (2^pred b, pred 2^b)

{-# INLINE genRndSafePrime #-}
genRndSafePrime :: BitSize -> IO Integer
genRndSafePrime b = let t | b > 3071 = 64 | b > 2047 = 56 | otherwise = 40 in
    fix $ \loop -> uncurry (bool loop) . first (return . fst) . second snd . dupe =<< 
            second (uncurry (&&) . first (millerRabin' t . (`shiftR` 1) . pred) . second (millerRabin' t) . dupe) .
                dupe . (.|. 1) <$> randomRIO (2^pred b, pred 2^b)

{-# INLINE rootFromSafePrime #-}
rootFromSafePrime :: Integer -> Integer
rootFromSafePrime p = head [g | g <- [2..p-2], modExp g (pred p `shiftR` 1) p /= 1]

{-# INLINE genKeys #-}
genKeys :: Integer -> Integer -> IO Keys
genKeys p g = (\(y, a) -> ((p, g, y), a)) . first (flip (modExp g) p) . dupe <$> randomRIO (1, pred p)
 
{-# INLINE encode #-}
encode :: PublicKey -> String -> IO Cryptogram
encode _ [] = return []
encode (p, g, y) plain = concat <$> mapM (\c -> 
    uncurry (:) . first (flip (modExp g) p) . 
        second ((:[]) . flip (`modExp` 1) p . (toInteger (ord c) *) . flip (modExp y) p) . 
            dupe . toInteger <$> randomRIO (1, maxBound :: Int)) plain

decode :: Keys -> Cryptogram -> Maybe String
decode _ [] = Just []
decode _ [_] = Nothing
decode ((p, g, y), a) (x1:x2:xs) = case modExp x1 a p `modInv` p of
    (Just q) -> (:) <$> Just (chr (fromIntegral (modExp (x2 * q) 1 p))) <*> decode ((p, g, y), a) xs
    Nothing -> Nothing

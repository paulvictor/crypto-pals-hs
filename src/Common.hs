module Common where

import Data.Bits
import Data.List
import Data.Int

bitsToHex :: [Bool] -> Char
bitsToHex  [False, False, False, False] ='0'
bitsToHex  [False, False, False, True]  ='1'
bitsToHex  [False, False, True, False]  ='2'
bitsToHex  [False, False, True, True]   ='3'
bitsToHex  [False, True, False, False]  ='4'
bitsToHex  [False, True, False, True]   ='5'
bitsToHex  [False, True, True, False]   ='6'
bitsToHex  [False, True, True, True]    ='7'
bitsToHex  [True, False, False, False]  ='8'
bitsToHex  [True, False, False, True]   ='9'
bitsToHex  [True, False, True, False]   ='a'
bitsToHex  [True, False, True, True]    ='b'
bitsToHex  [True, True, False, False]   ='c'
bitsToHex  [True, True, False, True]    ='d'
bitsToHex  [True, True, True, False]    ='e'
bitsToHex  [True, True, True, True]     ='f'

hexToBits :: Char -> [Bool]
hexToBits '0' = [False, False, False, False]
hexToBits '1' = [False, False, False, True]
hexToBits '2' = [False, False, True, False]
hexToBits '3' = [False, False, True, True]
hexToBits '4' = [False, True, False, False]
hexToBits '5' = [False, True, False, True]
hexToBits '6' = [False, True, True, False]
hexToBits '7' = [False, True, True, True]
hexToBits '8' = [True, False, False, False]
hexToBits '9' = [True, False, False, True]
hexToBits 'a' = [True, False, True, False]
hexToBits 'b' = [True, False, True, True]
hexToBits 'c' = [True, True, False, False]
hexToBits 'd' = [True, True, False, True]
hexToBits 'e' = [True, True, True, False]
hexToBits _ =   [True, True, True, True]

boolToInt False = 0
boolToInt True = 1

hexToInt8 :: [Char] -> Int8
hexToInt8 (x1 : x2 : []) = sum $ zipWith (\e f -> (2::Int8)^e * f) [7,6..0] (boolToInt <$> (hexToBits x1) ++ (hexToBits x2))
hexToInt8 _ = undefined

{-hexStringToInt8s :: [Char] -> [Int8]-}
hexStringToInt8s s = ( hexToInt8 . fmap snd) <$> (groupBy (\t1 t2 -> fst t1 == fst t2) (zip ((\i -> quot i 2) <$> [0..]) s))

int8ToHexString :: Int8 -> [Char]
int8ToHexString i = bitsToHex <$> ((fmap $ testBit i) <$> [[7,6,5,4], [3,2,1,0]])

int8sToHexString :: [Int8] -> [Char]
int8sToHexString is = is >>= int8ToHexString


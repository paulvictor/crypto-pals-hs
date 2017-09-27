module Challenge1 (base64) where

import Common
import Data.Bits
import Data.List

bits xs = map (\b -> if b then 1 else 0) bitBools
          where bitBools = ((map fromEnum xs) ++ (case (length xs `rem` 3) of
                                                    1 -> replicate 2 0
                                                    2 -> replicate 1 0
                                                    _ -> [])) >>= (\i -> map (testBit i) [7,6..0])
bitsFromHex xs = boolToInt <$> ((xs ++ (case (length xs `rem` 6) of 1 -> replicate 4 '0'
                                                                    2 -> replicate 4 '0'
                                                                    3 -> replicate 2 '0'
                                                                    4 -> replicate 2 '0'
                                                                    5 -> replicate 1 '0'
                                                                    _ -> [])) >>= hexToBits)

{-base64 :: [Char] -> [Char]-}
base64 xs = let ref = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/']
                indexes = map (\bits -> sum $ zipWith (\i j -> 2^i * j) [5,4..0] bits ) (map (map fst) $ groupBy (\t1 t2 -> (snd t1) == (snd t2)) (zip (bitsFromHex xs) ([0..] >>= (replicate 6))))
              in map (\i -> ref !! i) indexes


module Challenge2 where

import qualified Data.Bits as Bits
import Common

xor s1 s2 = int8sToHexString $ zipWith Bits.xor (hexStringToInt8s s1) (hexStringToInt8s s2)

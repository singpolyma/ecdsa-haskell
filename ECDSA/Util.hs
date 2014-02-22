{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_integer_gmp
#define MIN_VERSION_integer_gmp(a,b,c) 0
#endif
#if MIN_VERSION_integer_gmp(0,5,1)
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
#endif
module ECDSA.Util (ECDSA.Util.pmul, lengthBytes) where

import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))
import Codec.Crypto.ECC.Base as Hecc (EC(ECi), ECPF(ECPa), getx, gety, pmul)

#if MIN_VERSION_integer_gmp(0,5,1)
import GHC.Integer.GMP.Internals
#else
import Data.Bits
#endif

pmul :: (Curve, Point) -> Integer -> Point
pmul (c,p) = hecc2point . Hecc.pmul (point2hecc c p)

curve2hecc :: Curve -> EC Integer
curve2hecc (CurveFP (CurvePrime p (CurveCommon a b _ n 1))) =
	ECi (8 * lengthBytes n) a b p n
curve2hecc _ = error "TODO: binary curves"

point2hecc :: Curve -> Point -> ECPF Integer
point2hecc curve (Point x y) = ECPa (curve2hecc curve) x y
point2hecc _ _ = error "Point at infinity cannot be represented"

hecc2point :: ECPF Integer -> Point
hecc2point p = Point (getx p) (gety p)

-- crypto-numbers has this, but depends on crypto-random
lengthBytes :: Integer -> Int
#if MIN_VERSION_integer_gmp(0,5,1)
lengthBytes n = I# (word2Int# (sizeInBaseInteger n 256#))
#else
lengthBytes n
    | n < 256   = 1
    | otherwise = 1 + lengthBytes (n `shiftR` 8)
#endif

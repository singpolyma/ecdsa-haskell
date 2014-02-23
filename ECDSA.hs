module ECDSA (
	Signature(..),
	PublicKey(..),
	PrivateKey(..),
	sign,
	publicFromPrivate,
	publicToBytes,
	signatureEncodeDER
) where

import Data.Bits
import qualified Data.ByteString as BS

import Crypto.Types.PubKey.ECDSA (Signature(..), PrivateKey(..), PublicKey(..))
import Crypto.Types.PubKey.ECC (Curve(CurveFP), CurvePrime(..), CurveCommon(..), Point(..))

import Codec.Crypto.ECC.Base (modinv)

import Crypto.Random (genBytes, GenError, CryptoRandomGen)
import Crypto.Util (bs2i, i2bs, i2bs_unsized)

import ECDSA.Util

sign :: (CryptoRandomGen g) => PrivateKey -> BS.ByteString -> g -> Either GenError (Signature, g)
sign (PrivateKey curve@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g, ecc_n = n}))) d) hash gen = do
	(bytes, gen') <- genBytes (lengthBytes n) gen
	let k = (bs2i bytes `mod` (n-1)) + 1
	let r = (\(Point x _) -> x) (pmul (curve, g) k) `mod` n
	let s = ((bs2i hash + (r*d)) * modinv k n) `mod` n
	return (Signature r s, gen')
	where
sign _ _ _ = error "TODO: binary curves"

publicFromPrivate :: PrivateKey -> PublicKey
publicFromPrivate (PrivateKey curve@(CurveFP (CurvePrime _ (CurveCommon {ecc_g = g}))) d) =
	PublicKey curve (pmul (curve, g) d)
publicFromPrivate _ = error "TODO: binary curves"

-- This is used in Ripple, not sure if standard
publicToBytes :: PublicKey -> BS.ByteString
publicToBytes (PublicKey (CurveFP (CurvePrime _ (CurveCommon {ecc_n = n}))) (Point x y)) =
	BS.singleton (if y `mod` 2 == 0 then 0x02 else 0x03)
	`BS.append`
	i2bs (8 * lengthBytes n) x
publicToBytes _ = error "TODO: binary curves"

signatureEncodeDER :: Signature -> BS.ByteString
signatureEncodeDER (Signature r s) = BS.concat [
		BS.singleton 0x30,
		BS.singleton (fromIntegral $ 4 + BS.length rb' + BS.length sb'),
		BS.singleton 0x02,
		BS.singleton (fromIntegral $ BS.length rb'),
		rb',
		BS.singleton 0x02,
		BS.singleton (fromIntegral $ BS.length sb'),
		sb'
	]
	where
	-- If high bit is set, prepend an extra zero byte (DER signed integer)
	rb' | BS.head rb .&. 0x80 /= 0 = 0 `BS.cons` rb
	    | otherwise = rb

	sb' | BS.head sb .&. 0x80 /= 0 = 0 `BS.cons` sb
	    | otherwise = sb

	rb = i2bs_unsized r
	sb = i2bs_unsized s

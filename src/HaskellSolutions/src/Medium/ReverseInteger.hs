module Medium.ReverseInteger where

import           Data.Bool (bool)
import           Data.List (isPrefixOf)

reverseInteger :: Integer -> Integer
reverseInteger int
  | int < (-2 ^ 31) || int >= (2 ^ 31) = 0
  | otherwise = (reverseInt . show) int
  where
    reverseInt x =
      bool (read x) ((negate . read . reverse . tail) x) ("-" `isPrefixOf` x)
--
-- $> reverseInteger (-2^31)

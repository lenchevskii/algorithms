module Easy.MergeLists where

import           Data.Bool (bool)

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists l1@(x:xs) l2@(y:ys) =
  bool (y : mergeLists l1 ys) (x : mergeLists xs l2) (x < y)
--
-- $> mergeLists [1,3,4,6,10,10] [1,2,5,8]

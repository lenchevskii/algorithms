module Easy.TwoSum where

import           Data.Bool (bool)

twoSum :: [Int] -> Int -> [Int]
twoSum nums y = twoSumR (zip nums [0 ..])
  where
    twoSumR :: [(Int, Int)] -> [Int]
    twoSumR [] = []
    twoSumR (x1:xs) = bool answer (twoSumR xs) (null answer)
      where
        answer = twoSumH (fst x1 +) xs
        twoSumH f []      = []
        twoSumH f (x2:xs) = bool (twoSumH f xs) [snd x1, snd x2] (f (fst x2) == y)
--
-- $> twoSum [1,2,3,4,10,11,12,2,3] 5

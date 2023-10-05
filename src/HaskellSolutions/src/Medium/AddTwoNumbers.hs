module Medium.AddTwoNumbers where

import           Data.Bool (bool)

addTwoNumbers :: [Int] -> [Int] -> [Int]
addTwoNumbers l1 l2 =
  case (l1, l2) of
    ([], []) -> []
    ([x], []) -> [x]
    ([], [y]) -> [y]
    (x:xs, y:ys)
      | number < 10 -> number : addTwoNumbers xs ys
      | otherwise ->
        lastNumeral : addTwoNumbers (head xs + restNumber : tail xs) ys
      where restNumber = (read . init . show) number
            lastNumeral = (read . (: []) . last . show) number
            number = x + y
--
-- $> addTwoNumbers [2,4,3] [5,6,4] -- Output: [7,0,8]

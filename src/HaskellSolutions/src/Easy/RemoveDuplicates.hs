module Easy.RemoveDuplicates where

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l =
  case l of
    [] -> []
    [x] -> [x]
    (x:xs)
      | x == head xs -> removeDuplicates xs
      | otherwise -> x : removeDuplicates xs
--
-- $> removeDuplicates [1,2,2,3,4,4,5,5,6]

module Medium.LongestSubstring where

-- import           Data.Bool (bool)

longestSubstring' :: [Char] -> [Char]
longestSubstring' [] = []
longestSubstring' l = go l []
  where
    go [] [] = []
    go [] _ = []
    go (x:xs) stack
      | x `notElem` stack = x : longestSubstring' (go xs (x : stack))
      | otherwise = []

-- | Find the length of the longest substring `l` without repeating characters
longestSubstring :: [Char] -> Int
longestSubstring = length . longestSubstring'

-- longestSubstring :: [Char] -> [Char]
-- longestSubstring = foldr (\x acc -> bool (x : acc) acc (x `elem` acc)) []
--
-- $> longestSubstring "pwwkew"

module Medium.LongestSubstring where

import qualified Data.Map as Map

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

-- | Uses the two-pointer algorithm. ChatGPT generated.
lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring s = go 0 0 0 Map.empty
  where
    go start end maxLength charMap
      | end == length s = maxLength
      | otherwise =
        let currentChar = s !! end
         in case Map.lookup currentChar charMap of
              Just prevIndex ->
                let newStart = max prevIndex start + 1
                 in go
                      newStart
                      (end + 1)
                      (max maxLength (end - newStart + 1))
                      (Map.insert currentChar end charMap)
              Nothing ->
                go
                  start
                  (end + 1)
                  (max maxLength (end - start + 1))
                  (Map.insert currentChar end charMap)
--
-- $> lengthOfLongestSubstring "pwwkew"

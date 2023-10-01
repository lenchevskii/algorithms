module Easy.ReverseWords where

reverseWords :: String -> [String]
reverseWords = map reverse . words
--
-- $> reverseWords "Let's take LeetCode contest"

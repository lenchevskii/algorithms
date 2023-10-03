module Easy.LongestCommonPrefix where

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 longestCommonPrefixH
  where
    longestCommonPrefixH :: String -> String -> String
    longestCommonPrefixH _ [] = []
    longestCommonPrefixH [] _ = []
    longestCommonPrefixH (y:ys) (z:zs)
      | y == z = y : longestCommonPrefixH ys zs
      | otherwise = []
--
-- $> longestCommonPrefix ["flower","flow","flight"]

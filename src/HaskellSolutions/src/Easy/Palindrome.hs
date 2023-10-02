module Easy.Palindrome where

palindrome :: Int -> Bool
palindrome i = show i == (reverse . show) i
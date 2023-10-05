module Easy.ValidParentheses where

import           Data.Map as Map (Map, elems, fromList, member)

type Brackets = Map Char Char

brackets :: Brackets
brackets = Map.fromList [(')', '('), ('}', '{'), (']', '[')]

isValid :: String -> Bool
isValid = isValid' []

isValid' :: String -> [Char] -> Bool
isValid' [] [] = True
isValid' _ [] = False
isValid' stack (x:xs)
  | isOpeningBracket x = isValid' (x : stack) xs
  | isClosingBracket x =
    case stack of
      []     -> False
      (y:ys) -> isClosingBracket x && isValid' ys xs
  | otherwise = isValid' stack xs
  where
    isOpeningBracket = (`elem` Map.elems brackets)
    isClosingBracket = flip Map.member brackets
--
-- $> isValid "{{{()}}}[]"

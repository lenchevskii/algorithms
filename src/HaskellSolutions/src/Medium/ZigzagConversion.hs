module Medium.ZigzagConversion where

-- | Test function.
zigzagConversion :: String -> Int -> String
zigzagConversion s numRows =
  case (s, numRows) of
    ([], numRows) -> []
    (x:xs, numRows)
      | numRows > 1 -> replicate (numRows - 2) ' ' ++ [x]
      | otherwise -> []
--
-- Input: s = "PAYPALISHIRING", numRows = 4
-- Output: "PINALSIGYAHRPI"
-- Explanation:
-- P     I    N
-- A   L S  I G
-- Y A   H R   
-- P     I     
--
-- Input: s = "PAYPALISHIRING", numRows = 5
-- Output: "PHASIYIRPLIGAN"
-- Explanation:
-- P       H  
-- A     S I  
-- Y   I   R  
-- P L     I G
-- A       N  

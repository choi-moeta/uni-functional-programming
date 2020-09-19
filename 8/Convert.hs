-- 1 пару круглых скобок считать кавычками
-- 2 тесты ^
-- 3 профилирование

module Convert where

import Data.Char (toUpper, toLower, ord)

-- swaps case for english letters, change tabs to two spaces, trim tail spaces, only not in quotes
convert :: String -> String
convert str = trim $ convert' str False False False
    where
        convert' :: String -> Bool -> Bool -> Bool -> String
        convert' [] _ _ _ = []
        convert' (x:xs) inQuotes inApostrophes inDoubleBkt
            -- Quote check
            | x == '\''             = x : convert' xs (not inQuotes) inApostrophes inDoubleBkt
            | inQuotes == True      = x : convert' xs inQuotes inApostrophes inDoubleBkt
            | x == '`'              = x : convert' xs inQuotes (not inApostrophes) inDoubleBkt
            | inApostrophes == True = x : convert' xs inQuotes inApostrophes inDoubleBkt
            | x == ')'              = x : convert' xs inQuotes inApostrophes False
            | x == '('              = x : convert' xs inQuotes inApostrophes True
            | inDoubleBkt == True   = x : convert' xs inQuotes inApostrophes inDoubleBkt
            -- Case swap
            | x <= 'z' && x >= 'a' = toUpper x : convert' xs inQuotes inApostrophes inDoubleBkt
            | x <= 'Z' && x >= 'A' = toLower x : convert' xs inQuotes inApostrophes inDoubleBkt
            -- Tab to 2 spaces swap
            | x == '\t'            = "  " ++ convert' xs inQuotes inApostrophes inDoubleBkt
            -- Change nothing
            | otherwise            = x : convert' xs inQuotes inApostrophes inDoubleBkt

        trim :: String -> String
        trim str
            | length str == 0 = ""
            | last str == ' ' = trim $ init str
            | otherwise = str
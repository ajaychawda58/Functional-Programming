module Char (equal, isNumeral, isBlank, shift)
where
import Data.Char

--------------------------------------------------------------------------------
-- a)

equal :: String -> String -> Bool
equal a b = (map toLower a == map toLower b)

--------------------------------------------------------------------------------
-- b)

isNumeral :: String -> Bool
isNumeral input = 
    and (map isDigit input)

isBlank :: String -> Bool
isBlank input =
    and (map isSpace input)


--------------------------------------------------------------------------------
-- c)

shift :: Int -> Char -> Char
shift x y
      | isUpper y = toUpper(chr(((ord y + x - 65) `mod` 26)+65))
      | x<0 = toUpper(chr(((ord y - x*(-1) - 65) `mod` 26) + 65))
      | otherwise = y


msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ "
   ++ "JHLJBZ KPJABT HYJHUBT LZA ULBAYVU"

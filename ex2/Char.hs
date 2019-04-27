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
isNumeral = undefined

isBlank :: String -> Bool
isBlank = undefined

--------------------------------------------------------------------------------
-- c)

shift :: Int -> Char -> Char
shift = undefined

msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ "
   ++ "JHLJBZ KPJABT HYJHUBT LZA ULBAYVU"

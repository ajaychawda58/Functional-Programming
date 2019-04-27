module Char (equal, isNumeral, isBlank, shift)
where
import Data.Char

--------------------------------------------------------------------------------
-- a)

equal :: String -> String -> Bool
equal a b = (map toLower a == map toLower b)

--------------------------------------------------------------------------------
-- b)

isNum :: Char -> Bool
isNum x
    | x `elem` ['0'..'9'] = True
    | otherwise = False

isNumeral :: String -> Bool
isNumeral a = all isNum a

isBLANK :: Char -> Bool
isBLANK x = isSpace x

isBlank :: String -> Bool
isBlank x = all isBLANK x


--------------------------------------------------------------------------------
-- c)

shift :: Int -> Char -> Char
shift = undefined

msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ "
   ++ "JHLJBZ KPJABT HYJHUBT LZA ULBAYVU"

module Poem (thisOldMan)
where

thisOldMan :: String
thisOldMan = 
    drop 2 (concat (map (\(x, y) -> buildParagraph x y) (mergeList firstLineEndings secondLineEndings)))

firstLinePrefix = "\n\nThis old man, he played "
firstLineEndings = ["one","two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

seconLinePrefix = "\nHe played knick-knack "
secondLineEndings = ["on my thumb","on my shoe", "on my knee", "on my door", "on my hive", "on my sticks", "up in heaven", "on my gate", "on my spine", "once again"]

lastLines = "\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home."

buildParagraph :: String -> String -> String
buildParagraph firstLineEnding secondLineEnding =
    firstLinePrefix ++ firstLineEnding ++ "," ++ seconLinePrefix ++ secondLineEnding ++ ";" ++ lastLines

-- Zip for string list 
mergeList :: [String] -> [String] -> [(String, String)]
mergeList (x:xs) (y:ys) = (x, y) : mergeList xs ys   
mergeList _ _ = [] 

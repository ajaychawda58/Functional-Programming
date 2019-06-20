-- Compile with: ghc --make WordCount.hs

module Main
where
import System.Environment

main :: IO ()
--main = do
--    args <- getArgs
--    putStrLn "To be done..."
countLines :: String -> Int
countLines str = length.lines $ str

countWords :: String -> Int
countWords str=length.words $ str

countBytes :: String -> Int
countBytes str=length $ str

conv :: [Char] -> String
conv s = s

main = do
    [args] <- getArgs
    contents <- readFile args
    let x=conv contents
    let fileName=conv args
    let a=countLines x
    let b=countWords x
    let c=countBytes x
    putStrLn ((show (a)) ++ " " ++ (show (b)) ++ " " ++ (show (c))++ " " ++ fileName ++ "\n")
    
    
{-
For multiple arguments, this is used
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn " The arguments are : "
    mapM putStrLn getArgs
    putStrLn " The program name is : "
    putStrLn progName

I could not do it for multiple files.
-}

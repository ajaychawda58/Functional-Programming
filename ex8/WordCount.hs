-- Compile with: ghc --make WordCount.hs

module Main
where
import System.Environment

main :: IO ()

countLines :: String -> Int
countLines str = length.lines $ str

countWords :: String -> Int
countWords str=length.words $ str

countBytes :: String -> Int
countBytes str=length $ str

prettyPrint l w b f =
    putStrLn ("\t" ++ show l  ++ "\t" ++ show w ++ "\t" ++ show b ++ "\t" ++ f)

countFile :: [Char] -> IO (Int, Int, Int)
countFile fileName =
    do
    content <- readFile fileName
    let l = countLines content
    let w = countWords content
    let b = countBytes content
    prettyPrint l w b fileName
    return (l , w , b)

validateInput args
    | len == 0 = error "No argument provided"
    | otherwise = return ()
    where len = length args

main = do
    args <- getArgs
    validateInput args
    contents <- readFile (head args)
    res <- mapM (\x -> countFile x) args
    let (a, b, c) = foldl (\(l, w, b) (xl, xw, xb) -> (l+xl, w+xw , b+xb)) (0, 0 ,0) res
    case length args of
        1 -> return ()
        _ -> prettyPrint a b c "total"
 

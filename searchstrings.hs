
-- stack runhaskell searchstrings.hs
-- echo "foo bar foobar helloworld hello world" | stack runhaskell searchstrings.hs
-- stack runhaskell searchstrings.hs < ledger.journal
-- grep ^20 ledger.journal | stack runhaskell searchstrings.hs | grep -v PROVIDED | paste -sd'|'

import System.IO
import Data.List

main = do
    text <- getContents
    let wss = map myWords $ lines text
        allWords = concat wss
        wordPairs = concatMap (\ws -> zip ws $ tail ws) wss
        wordPairs' = filter (\(x, y) -> x ++ y `elem` allWords) wordPairs
        searchStrings = map (\(x, y) -> "<" ++ x ++ "\\zs +" ++ y ++ ">") wordPairs'
    mapM_ putStrLn searchStrings

myWords :: String -> [String]
myWords = foldr f []

--myWords = words
-- words hat das Problem, dass es im CSV z.B. dieses;Wort nicht findet -> zweiter durchgang im ledger format oder words ändern

f :: Char -> [String] -> [String]
f c []      | c `elem` " -.,:!?()_" = []
            | otherwise             = [[c]]
f c (a:ccu) | c `elem` " -.,:!?()_" = if null a then a:ccu else []:a:ccu
            | otherwise             = (c:a):ccu

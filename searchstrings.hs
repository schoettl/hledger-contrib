
-- stack runhaskell searchstrings.hs
-- echo "foo bar foobar helloworld hello world" | stack runhaskell searchstrings.hs
-- stack runhaskell searchstrings.hs < ledger.journal
-- grep ^20 ledger.journal | stack runhaskell searchstrings.hs | grep -v PROVIDED | paste -sd'|'

import System.IO
import Data.List

main = do
    text <- getContents
    let wss = map words $ lines text
        allWords = concat wss
        wordPairs = concatMap (\ws -> zip ws $ tail ws) wss
        wordPairs' = filter (\(x, y) -> x ++ y `elem` allWords) wordPairs
        searchStrings = map (\(x, y) -> "<" ++ x ++ "\\zs +" ++ y ++ ">") wordPairs'
    mapM_ putStrLn searchStrings

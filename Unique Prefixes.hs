import System.IO
import Control.Monad


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int

    words <- replicateM n getLine

    let prefixes = map (\x -> find x words) words

    mapM_ putStrLn prefixes

find :: String -> [String] -> String
find word list = find' word list 1
    where
        find' w l len
            | length (filter (\x -> take len x == take len w) l) == 1 = take len w
            | otherwise = find' w l (len + 1)

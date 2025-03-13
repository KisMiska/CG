import System.IO
import Control.Monad
import Data.Bits

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let n = read input_line :: Int
    

    replicateM_ n $ do
        input_line <- getLine
        let input = words input_line
        let a = read (input !! 0) :: Int
        let b = read (input !! 1) :: Int
        
        let c = (a + b) `div` gcd a b
        putStrLn (solve c)

solve c
    | c .&. (c - 1) == 0 = "halts"
    | otherwise          = "loops"


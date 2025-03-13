import System.IO
import Control.Monad
import Data.Char
import Control.Monad.Trans.Cont (reset)
import Data.List (elemIndex)

caesar off x = chr ((ord x - ord 'A' + off) `mod` 26 + ord 'A')

revcaesar off x = chr  ((ord x - ord 'A' - off + 26) `mod` 26 + ord 'A')

rotorate rotor x = rotor !! (ord x - ord 'A')

revrotorate rotor x = res
  where
    ind = findindex x rotor
    res = chr (ind + ord 'A')

findindex c (x:xs)
  | x == c = 0
  | otherwise = 1 + findindex c xs

enigma "ENCODE" n rotor1 rotor2 rotor3 txt = codedtext
  where
    s1 = map (\(off, c) -> caesar off c) (zip [n..] txt)
    s2 = map (rotorate rotor1) s1
    s3 = map (rotorate rotor2) s2
    s4 = map (rotorate rotor3) s3
    codedtext = s4

enigma "DECODE" n rotor1 rotor2 rotor3 txt = decodedtext
  where
    s1 = map (revrotorate rotor3) txt
    s2 = map (revrotorate rotor2) s1
    s3 = map (revrotorate rotor1) s2
    s4 = map (\(off, c) -> revcaesar off c) (zip [n..] s3)
    decodedtext = s4


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    operation <- getLine
    n <- readLn
    rotor1 <- getLine
    rotor2 <- getLine
    rotor3 <- getLine
    message <- getLine
   
    putStrLn $ enigma operation n rotor1 rotor2 rotor3 message

    return ()
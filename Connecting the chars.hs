import System.IO
import Control.Monad
import Data.List
import Data.Char

distance [] [] = 0
distance (x:xs) (y:ys) = (x - y) ^ 2 + distance xs ys

getsignum [] = []
getsignum (x:xs)
  | x >= 0     = 1 : getsignum xs
  | otherwise = -1 : getsignum xs

findneighbour current points = findneighbour' points (head points)
  where
    findneighbour' [] nearest = nearest
    findneighbour' (x:xs) nearest
        | distance current (snd x) < distance current (snd nearest) = findneighbour' xs x
        | otherwise = findneighbour' xs nearest

space prevsign newsign
  | prevsign /= newsign = " "
  | otherwise = ""

solve _ [] _ = ""
solve current points prevsign = space prevsign newsign ++ [label] ++ solve coords remaining newsign
  where
    (label, coords) = findneighbour current points
    remaining = delete (label, coords) points
    newsign = getsignum coords

toint [] = 0
toint ('-':xs) = -(toint xs)
toint s = toint' s 0
  where
    toint' [] acc = acc
    toint' (x:xs) acc = toint' xs (acc * 10 + (ord x - ord '0'))

inputhandle = map (\ line -> (head line, map toint (tail (words line))))


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let count = read (input !! 0) :: Int
    let n = read (input !! 1) :: Int

    points <- replicateM count $ do
        labeledpoint <- getLine
        return labeledpoint

    let ppoints = inputhandle points
    let origin = replicate n 0
    let phrase = solve origin ppoints origin

    putStrLn (unwords ( words phrase))

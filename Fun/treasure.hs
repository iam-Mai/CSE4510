{-
 - Author1: Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:
 -}

module Main where

import System.IO
import Text.Printf
import Data.Char

main = interact (showResults . subMax .  compute . (map read) . words)

compute :: [Int] -> [(Int,Int,Int)]
compute [] = []
compute (x:xs) = pairUp xs

readNumbers :: String -> [Int]
readNumbers = map read . words

pairUp :: [Int] -> [(Int,Int,Int)]
pairUp [] = []
pairUp (n:m:o:rest) = (n,m,o) : (pairUp rest)
pairUp (_) = error "bad input" -- must multiple of 4 numbers in input

showResults ::  [(Int,Int,Int)] -> String
showResults = unlines . (map format)

format :: (Int,Int,Int) -> String
format (a, b, c) = printf "\n%d %d %d" a b c

findMax :: [(Int,Int,Int)] -> Int
findMax [] = 0
findMax list = maximum x
 where x = [1,2,3,4]

makeList :: [(Int,Int,Int)] -> [Int]
makeList [] = [0]
makeList (x:xs) = listFromTrip x ++ makeList xs

listFromTrip :: (Int,Int,Int) -> [Int]
listFromTrip (a,b,c) = [a,b,c]

sub :: Int -> (Int,Int,Int) -> (Int,Int,Int)
sub m (a,b,c) = (m-a, m-b, m-c)

subMax :: [(Int,Int,Int)] -> [(Int,Int,Int)]
subMax [] = []
subMax (x:xs) = [sub m x] ++ subMax xs
 where m = findMax (x:xs)


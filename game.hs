{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  CSE 4510, Fall 2017
 - Project: The Triangle Game
 -}

module Main where

import Text.Printf

data Shape = Triangle Integer Integer Integer

main :: IO()
main = interact (showResults . findSequence. readTestCases)

readTestCases :: String -> [[Integer]]
readTestCases = take 6 . setData . (map read) . words

setData :: [Integer] -> [[Integer]]
setData (0:_) = []
setData (n:m:o:rest) = [n,m,o] : (setData rest)

showResults ::  Integer -> String
showResults x = printf "%d" x

expandList ::  [[Integer]] -> [[[Integer]]]
expandList list = [list, []]

findSequence :: [[Integer]] -> Integer
findSequence list = ans
    where   x = calculate list []
            ans = getSum 0 x

calculate :: [[Integer]] -> [[Integer]] -> [[Integer]]
calculate (x:xs) list
    | a == [0,0,0]  = []
    | otherwise = list ++ a  : calculate xs list
    where   a = findB b list
            b = getMiddleElem x

findB :: Integer -> [[Integer]] -> [Integer]
findB b (x:xs)
    | x==[] = [0,0,0]
    | x!!0 == b = x
    | x!!1 == b = rotate2 x
    | x!!2 == b = rotate1 x
    | otherwise = findB b xs

getMiddleElem ::  [Integer] -> Integer
getMiddleElem [a,b,c] = b

getSum :: Integer -> [[Integer]] -> Integer
getSum s (x:xs)
    | x == [] = s
    | otherwise = getSum a xs
    where a = s + x!!2

rotate1 :: [Integer] -> [Integer]
rotate1 [a,b,c] = [c,a,b]

rotate2 :: [Integer] -> [Integer]
rotate2 [a,b,c] = [b,c,a]

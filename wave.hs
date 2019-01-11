{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  CSE 4510, Fall 2017
 - Project: Delta Wave
 -}

module Main where

import Text.Printf

main :: IO()
main = interact (showResults . map calculate . readTestCases)

readTestCases :: String -> [(Integer,Integer,Integer,Integer)]
readTestCases = setList . (map read) . words

setList :: [Integer] -> [(Integer, Integer, Integer, Integer)]
setList (0:_) = []
setList (1:_) = []
setList (n:rest) = (n,1,1,1) : (setList rest)                   -- (n,1,1,0) is same as (n,k,t,sum)

showResults ::  [(Integer, Integer, Integer, Integer)] -> String
showResults = unlines . (map format)

-- Format answer modula 10^100
format :: (Integer, Integer, Integer, Integer) -> String
format (n,k,t,sum) = printf "%d" (sum `mod` (10^100))

-- calculate find the possible path for each k (k <= n/2) and sum up all paths
calculate :: (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
calculate (n,k,t,sum)
    | k+k <= n = calculate(n, k+1, t*((n-2*k+1)*(n-2*k+2)) `div` (k*(k+1)), sum+(t*((n-2*k+1)*(n-2*k+2)) `div` (k*(k+1))))
    | k+k > n = (n,k,t,sum)


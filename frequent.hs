{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  CSE 4510, Fall 2017
 - Project: Frequent
 -}

module Main where

import Text.Printf
import Data.List


main :: IO()
main = do
    let loop = do
        line <- getLine

        -- Convert String into Int
        let numbers = map read $ words line :: [Int]
        let n = numbers!!0

        --Input is terminated by n = 0
        if n == 0
        then return ()
            else do
            let q = numbers!!1
            line <- getLine
            let array = map read $ words line :: [Int]
            recall q array
            loop
    loop
    return ()

recall x array
    | x /= 0 = do
        suboperate array
        recall (x-1) array
    | otherwise = return ()


suboperate array = do
    line <- getLine
    let range = take 2 . map read $ words line :: [Int]
    let i = range!!0
    let j = range!!1
    let x = subarray i j array
    let y = maxSeqLength x
    putStrLn (show y)

showResults ::  [Int] -> String
showResults = unlines . (map format)

format :: Int -> String
format n = printf "%d " n

maxSeqLength :: Eq a => [a] -> Int
maxSeqLength [] = 0
maxSeqLength xs = (maximum . map length . group) xs

subarray :: Int -> Int -> [Int] -> [Int]
subarray i j array
    | j-i == 1 = take (j-1) $ drop (i-1) $ array
    | otherwise = take (j) $ drop (i-1) $ array



{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  CSE 4510, Fall 2017
 - Project: Delta Wave
 -}

module Main where

import Text.Printf

data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

main = do
    -- get input tree
    line <- getLine
    let elements = map read $ words line :: [Char]
    putStrLn elements!!0
    return ()

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

isEmpty :: Tree -> Bool
isEmpty Nil = True
isEmpty  _  = False

insert :: Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x
	| v == x = Node t1 v t2
	| v  < x = Node t1 v (insert t2 x)
	| v  > x = Node (insert t1 x) v t2

preorder :: Tree a -> [a]
preorder Nil = []
preorder (Node t1 v t2) = [v] ++ preorder t1 ++ preorder t2




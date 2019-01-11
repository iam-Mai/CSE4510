{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  cse4250, Fall 2017
 - Project: main
 -}

module Main where

import System.IO
import Text.Printf

data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Eq, Show)

main :: IO()
main = interact (unlines . getTree . words)

getTree :: [String] -> [String]
getTree [] = []
getTree (x:xs) =  decode t xs
 where t = inputTree Leaf x

decode :: Tree Char -> [String] -> [String]
decode _ [] = []
decode tr (x:xs) = [a] ++ decode tr xs
 where a = traversal tr tr x

makeTree :: Tree Char -> Char -> Tree Char
makeTree (Leaf) a = Branch a Leaf Leaf
makeTree (Branch v t1 t2) a
    | v /= '*' = Leaf
    | v == '*' && findSpace t1 =  Branch v (makeTree t1 a) t2
    | otherwise = Branch v t1 (makeTree t2 a)

--inputTree is used to construct a tree from String ([Char])
inputTree :: Tree Char -> [Char] -> Tree Char
inputTree tr [] = tr
inputTree tr (x:xs) = inputTree (makeTree tr x) xs

-- isComplete Checkif there is an available spot in the branch
findSpace :: Tree Char -> Bool
findSpace Leaf = True
findSpace (Branch v t1 t2)
    | v /= '*' = False
    | t1 == Leaf || t2 == Leaf = True
    | otherwise = findSpace t1 || findSpace t2

-- traversal get tree and list of int then decode this list of int and return list of char
traversal :: Tree Char -> Tree Char -> String -> String
traversal Leaf _ _ = []
traversal tr (Branch v Leaf Leaf) [] = [v]
traversal tr (Branch v Leaf Leaf) x = [v] ++ traversal tr tr x
traversal tr (Branch v l r) (x:xs)
 | x == '0' = a ++ traversal tr l xs
 | otherwise = a ++ traversal tr r xs
 where a | v == '*' = [] | otherwise = [v]


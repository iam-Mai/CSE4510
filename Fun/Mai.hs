{-
 - Author1: Sasithorn Hannarong, shannarong2015@fit.edu
 - Author2: Akash Chanda, achanda2015@my.fit.edu
 - Course: CSE 4250, Fall 2017
 - Project: Proj4, Decoding Text
 - Language implementation: Haskell
 -}

module Main where

import System.IO
import Text.Printf

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

main :: IO()
main = interact (unlines . compute . words)

compute :: [String] -> [String]
compute (x:xs) =  decode t xs
 where t = inputTree Nil x

decode :: Tree Char -> [String] -> [String]
decode _ [] = []
decode tr (x:xs) = [a] ++ decode tr xs
 where a = traversal tr tr x

-- insert is used to insert element in tree
insert :: Tree Char -> Char -> Tree Char
insert Nil a = Branch Nil a Nil -- empty tree
insert (Branch l v r) a
 | v == '*' && isFree l =  Branch (insert l a) v r    -- Look at the l side, if there is empty leaf, insert a in l
 | otherwise = Branch l v (insert r a)                -- Otherwise, insert a in r

--inputTree is used to construct a tree from String
inputTree :: Tree Char -> String -> Tree Char
inputTree tr [] = tr
inputTree tr (x:xs) = inputTree t1 xs
 where t1 = insert tr x

-- isFree check if there is an available spot in the branch
isFree :: Tree Char -> Bool
isFree Nil = True
isFree (Branch l v r)
 | v /= '*' = False              -- Node isn't *
 | l == Nil || r == Nil = True   -- Node * has 1 or 2 nil leaf
 | otherwise = isFree l || isFree r

-- traversal get tree and list of int then decode this list of int and return list of char
traversal :: Tree Char -> Tree Char -> String -> String
traversal Nil _ _ = []
traversal tr (Branch Nil v Nil) [] = [v]
traversal tr (Branch Nil v Nil) x = [v] ++ traversal tr tr x
traversal tr (Branch l v r) (x:xs)
 | x == '0' = a ++ traversal tr l xs
 | otherwise = a ++ traversal tr r xs
 where a | v == '*' = [] | otherwise = [v]

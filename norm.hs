{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  cse4250, Fall 2017
 - Project: Norm for question 7 in take home final
 -}

module Main where

import System.IO
import Text.Printf

data Var = P | Q | True | False  deriving (Show)
data Op =  Or Op Op | And Op Op | Var  deriving (Show)

main :: IO()
main = interact (unlines . words)

eval ::  Op  -> Bool
eval P (pv,qv) = pv
eval Q (pv,qv) = qv




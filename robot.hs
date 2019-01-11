{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  cse4250, Fall 2017
 - Project: Robot for question 6 in take home final
 -}

module Main where

import System.IO
import Text.Printf

data Command = Forward Int | Backward Int | TurnLeft | TurnRight
data Direction = UpW | BackW | LeftW | RightW

main :: IO()
main = interact (unlines . words)

destination :: [Command] -> (Int,Int)
destination [] = (0,0)
destination (x:xs) = calDestination (x:xs) UpW (0,0)
 where dir  = updateDir x UpW


calDestination :: [Command] -> Direction -> (Int,Int) -> (Int,Int)
calDestination [] _ (x,y) = (x,y)
calDestination (a:as) dir (x,y) = calDestination as (updateDir a dir) (updatePos a dir (x,y))

updateDir :: Command -> Direction -> Direction
updateDir (Forward _) a = a                                         -- Forward doesn't change direction
updateDir (Backward _) a = a                                        -- Backward doesn't change direction
updateDir TurnLeft UpW = LeftW
updateDir TurnLeft BackW = RightW
updateDir TurnLeft LeftW = BackW
updateDir TurnLeft RightW = UpW
updateDir TurnRight UpW = RightW
updateDir TurnRight BackW = LeftW
updateDir TurnRight LeftW = UpW
updateDir TurnRight RightW = BackW

updatePos :: Command -> Direction -> (Int,Int) -> (Int,Int)
updatePos (Forward a) UpW (x,y) = (x, y+a)
updatePos (Forward a) BackW (x,y) = (x, y-a)
updatePos (Forward a) RightW (x,y) = (x+a, y)
updatePos (Forward a) LeftW (x,y) = (x-a, y)
updatePos (Backward a) UpW (x,y) = (x, y-a)
updatePos (Backward a) BackW (x,y) = (x, y+a)
updatePos (Backward a) RightW (x,y) = (x-a, y)
updatePos (Backward a) LeftW (x,y) = (x+a, y)
updatePos _ _ (x,y) = (x,y)                                     -- TurnLeft | TurnRight doesn't change position

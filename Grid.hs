{-
 - Author:  Sasithorn Hannarong, shannarong2015@fit.edu
 - Course:  CSE 4510, Fall 2017
 - Project: Paths on a Grid
 -}

main = do
    let loop = do
        -- Input string
        line <- getLine

        -- Convert String into Int
        let numbers = map read $ words line :: [Int]
        let n = numbers!!0
        let m = numbers!!1

        --Input is terminated by n and m=0
        if n == 0 && m == 0
        then return ()
            else do
            --Find Paths on a Grid
            let result = (fac(n+m)) `div` (fac(n)*fac(m))
            let x = show result
            putStrLn x
            loop
    loop
    return ()

fac :: Int -> Int
fac n = product [1..n]

#!/usr/bin/env stack
-- stack --resolver lts-12.21 script


data BrainfuckOps = 
    MoveRight 
    | MoveLeft 
    | Increment 
    | Decrement 
    | Output
    | Input 
    | JumpForward 
    | JumpBack deriving (Show)


initCells::[Int]
initCells = init 30000
    where init::Int -> [Int]
          init n 
              | n == 0 = []
              | otherwise = 0 : init (n-1)

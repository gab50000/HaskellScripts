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

type State = [Int]

initCells::State
initCells = init 30000
    where init::Int -> State
          init n
              | n == 0 = []
              | otherwise = 0 : init (n-1)

type Pos = Int

executeCode::[BrainfuckOps] -> Pos -> State -> IO ()
executeCode [] pos state = return()
executeCode (MoveRight:ops) pos state
    | pos < length state = executeCode ops (pos + 1) state
    | otherwise = return()
executeCode (op:ops) pos state = case op of
    Increment   -> executeCode ops pos (incrementAt state pos 1)
    Decrement   -> executeCode ops pos (incrementAt state pos (-1))
    JumpForward -> undefined
    JumpBack    -> undefined
    MoveLeft    -> executeCode ops (pos - 1) state
    MoveRight   ->  executeCode ops (pos + 1) state
    Output      -> undefined
    Input       -> undefined

    where incrementAt::State->Pos->Int->State
          incrementAt [] _ _       = []
          incrementAt (x:xs) 0 val = (x+val):xs
          incrementAt (x:xs) n val = x : incrementAt xs (n-1) val

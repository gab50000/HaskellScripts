#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BlockArguments #-}

import           Control.Monad (when)
import           Data.Char     (chr, ord)


data BrainfuckOps =
    MoveRight
    | MoveLeft
    | Increment
    | Decrement
    | Output
    | Input
    | JumpForward
    | JumpBack deriving (Show)

data Tape a = Tape [a] a [a] deriving (Show)

type State = Tape Int

-- initCells::State
-- initCells = replicate 30000 0

type Pos = Int

data Direction = Forward | Backward

advance :: Tape BrainfuckOps -> Direction -> Maybe(Tape BrainfuckOps)
advance (Tape _ _ []) Forward          = Nothing
advance (Tape [] _ _) Backward         = Nothing
advance (Tape l pivot (r:rs)) Forward  = Just(Tape (pivot:l) r rs)
advance (Tape (l:ls) pivot r) Backward = Just(Tape ls l (pivot:r))

increase:: State -> Int -> State
increase (Tape left cell right) val =  Tape left (cell+val) right

execCell :: BrainfuckOps -> State -> IO State
execCell Increment state                    = return (increase state 1)
execCell Decrement state                    = return (increase state $ -1)
execCell MoveLeft state                     = return state
execCell MoveRight state                    = return state
execCell Output state@(Tape _ cell _)       = (print . chr) cell >> return state
execCell Input state@(Tape left cell right) =  do
                                                char <- getChar
                                                return $ Tape left (ord char) right
execCell JumpForward state                  = return state
execCell JumpBack state                     = return state

executeCode::Maybe (Tape BrainfuckOps) -> State -> IO ()
executeCode Nothing                _           = return ()
executeCode (Just tape@(Tape _ Increment _)) state = executeCode (advance tape Forward) (increase state 1)
executeCode (Just tape@(Tape _ Decrement _)) state = executeCode (advance tape Forward) (increase state $ -1)
executeCode (Just tape@(Tape _ MoveLeft _)) state = undefined
executeCode (Just tape@(Tape _ MoveRight _)) state = undefined
executeCode (Just tape@(Tape _ Output _)) state = undefined
executeCode (Just tape@(Tape _ Input _)) state = undefined
executeCode (Just tape@(Tape _ JumpForward _)) state = undefined
executeCode (Just tape@(Tape _ JumpBack _)) state = undefined
-- executeCode tape
    -- MoveLeft  -> when (pos > 0) $ executeCode tape (pos - 1) state
    -- MoveRight -> when (pos > 0) $ executeCode tape (pos - 1) state
    -- Output    -> undefined
    -- Input     -> undefined

-- executeCode tape@(Tape left pivot (r:rs)) = executeCode (Tape pivot:left r rs)  state


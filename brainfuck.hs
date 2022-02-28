#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs          #-}

import           Control.Monad      (when)
import           Control.Monad.List (ListT)
import           Data.Char          (GeneralCategory (Format), chr, ord)
import           Data.List.NonEmpty as NE


data BrainfuckOps =
    MoveRight
    | MoveLeft
    | Increment
    | Decrement
    | Output
    | Input
    | JumpForward
    | JumpBack deriving (Show)

data Stream a = Stream a (Stream a) deriving Show

fillStream::Int -> Stream Int
fillStream x = Stream x (fillStream x)

data ListTape a = ListTape [a] a [a]
data StreamTape a = StreamTape (Stream a) a (Stream a)


advancel :: ListTape a -> Direction ->  Maybe (ListTape a)
advancel (ListTape _ _ []) Forward          = Nothing
advancel (ListTape [] _ _) Backward         = Nothing
advancel (ListTape l pivot (r:rs)) Forward  = Just $ ListTape (pivot:l) r rs
advancel (ListTape (l:ls) pivot r) Backward = Just $ ListTape ls l (pivot:r)

advances :: StreamTape a -> Direction -> StreamTape a
advances (StreamTape left pivot (Stream r rs)) Forward = StreamTape (Stream pivot left) r rs
advances (StreamTape (Stream l ls) pivot right) Backward = StreamTape ls l (Stream pivot right)

type State = StreamTape Int

initialState:: State
initialState = StreamTape (fillStream 0) 0 (fillStream 0)

type Pos = Int

data Direction = Forward | Backward

increase:: State -> Int -> State
increase (StreamTape left cell right) val =  StreamTape left (cell+val) right

execCell :: BrainfuckOps -> State -> IO State
execCell Increment state                    = return (increase state 1)
execCell Decrement state                    = return (increase state $ -1)
execCell MoveLeft state                     = return state
execCell MoveRight state                    = return state
execCell Output state@(StreamTape _ cell _)       = (print . chr) cell >> return state
execCell Input state@(StreamTape left cell right) =  do
                                                char <- getChar
                                                return $ StreamTape left (ord char) right
execCell JumpForward state                  = return state
execCell JumpBack state                     = return state

executeCode::Maybe (ListTape BrainfuckOps) -> State -> IO ()
executeCode Nothing _ = return ()
executeCode (Just tape@(ListTape _ Increment _)) state =
    execCell Increment state >>= executeCode (advancel tape Forward)
executeCode (Just tape@(ListTape _ Decrement _)) state =
    execCell Decrement state >>= executeCode (advancel tape Forward)
executeCode (Just tape@(ListTape _ MoveLeft _)) state =
        executeCode (advancel tape Forward) newState
        where
            newState = advances state Backward
executeCode (Just tape@(ListTape _ MoveRight _)) state =
        executeCode (advancel tape Forward) state
        where
            newState = advances state Forward
executeCode (Just tape@(ListTape _ Output _)) state =
    execCell Output state >>= executeCode (advancel tape Forward)
executeCode (Just tape@(ListTape _ Input _)) state =
    execCell Input state >>= executeCode (advancel tape Forward)
executeCode (Just tape@(ListTape _ JumpForward _)) state = undefined
executeCode (Just tape@(ListTape _ JumpBack _)) state = undefined
-- executeCode tape
    -- MoveLeft  -> when (pos > 0) $ executeCode tape (pos - 1) state
    -- MoveRight -> when (pos > 0) $ executeCode tape (pos - 1) state
    -- Output    -> undefined
    -- Input     -> undefined

-- executeCode tape@(Tape left pivot (r:rs)) = executeCode (Tape pivot:left r rs)  state


#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
import           Data.Char (GeneralCategory (Format), chr, ord)
import           System.IO (hFlush, stdout)


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


data ListTape a = ListTape [a] a [a] deriving (Show)


data StreamTape a = StreamTape (Stream a) a (Stream a) deriving (Show)


pushLeft::StreamTape a -> a -> StreamTape a
pushLeft (StreamTape left p right) val = StreamTape (Stream val left) p right


pushRight::StreamTape a -> a -> StreamTape a
pushRight (StreamTape left p right) val = StreamTape left p (Stream val right)


getCurrentCell::StreamTape a -> a
getCurrentCell (StreamTape _ x _) = x


advancel :: ListTape a -> Direction ->  Maybe (ListTape a)
advancel (ListTape _ _ []) Forward          = Nothing
advancel (ListTape [] _ _) Backward         = Nothing
advancel (ListTape l pivot (r:rs)) Forward  = Just $ ListTape (pivot:l) r rs
advancel (ListTape (l:ls) pivot r) Backward = Just $ ListTape ls l (pivot:r)


advances :: StreamTape a -> Direction -> StreamTape a
advances (StreamTape left pivot (Stream r rs)) Forward = StreamTape (Stream pivot left) r rs
advances (StreamTape (Stream l ls) pivot right) Backward = StreamTape ls l (Stream pivot right)


type State = StreamTape Int


type Source = ListTape BrainfuckOps


initialState::State
initialState = StreamTape (fillStream 0) 0 (fillStream 0)


bogusState::State
bogusState = pushRight (increase (pushLeft initialState 1) 2) 3


type Pos = Int


data Direction = Forward | Backward


increase:: State -> Int -> State
increase (StreamTape left cell right) val =  StreamTape left (cell+val) right


execCell :: BrainfuckOps -> State -> IO State
execCell Increment state                    = return (increase state 1)
execCell Decrement state                    = return (increase state $ -1)
execCell MoveLeft state                     = return (advances state Backward)
execCell MoveRight state                    = return (advances state Forward)
execCell Output state@(StreamTape _ cell _) = (putChar . chr) cell >> hFlush stdout >> return state
execCell Input state@(StreamTape left cell right) = do
                                                char <- getChar
                                                return $ StreamTape left (ord char) right
execCell JumpForward state                  = return state
execCell JumpBack state                     = return state


executeCode::Maybe Source -> State -> IO ()
executeCode Nothing _ = return ()

executeCode (Just tape@(ListTape _ JumpForward _)) state@(StreamTape _ cell _) =
    executeCode tape' state
    where tape' = if cell == 0 then jumpToMatchingBracket tape Forward else advancel tape Forward

executeCode (Just tape@(ListTape _ JumpBack _)) state@(StreamTape _ cell _) =
    executeCode tape' state
    where tape' = if cell == 0 then advancel tape Forward else jumpToMatchingBracket tape Backward

executeCode (Just tape@(ListTape _ cmd _)) state =
    execCell cmd state >>= executeCode (advancel tape Forward)


type Depth = Int


jumpToMatchingBracket::Source -> Direction -> Maybe Source
jumpToMatchingBracket tape@(ListTape l JumpForward r) Forward = jump (advancel tape Forward) Forward 1
jumpToMatchingBracket tape@(ListTape l JumpBack r) Backward = jump (advancel tape Backward) Backward 1
jumpToMatchingBracket tape _ = Nothing


jump::Maybe Source->Direction->Depth -> Maybe Source
jump Nothing _ _                                          = Nothing
jump (Just tape@(ListTape _ JumpForward _)) Forward depth = jump (advancel tape Forward) Forward (depth+1)
jump (Just tape@(ListTape _ JumpBack _)) Forward 1 = Just tape
jump (Just tape@(ListTape _ JumpBack _)) Forward depth = if depth < 0 then Nothing else jump (advancel tape Forward) Forward (depth-1)

jump (Just tape@(ListTape _ JumpBack _)) Backward depth = jump (advancel tape Backward) Backward (depth+1)
jump (Just tape@(ListTape _ JumpForward _)) Backward 1 = Just tape
jump (Just tape@(ListTape _ JumpForward _)) Backward depth = if depth < 0 then Nothing else jump (advancel tape Backward) Backward (depth-1)
jump (Just tape@(ListTape _ other _)) dir depth = jump (advancel tape dir) dir depth


parseSource:: String -> [BrainfuckOps]
parseSource [] = []
parseSource (x:xs) = case maybeParsedChar of
    Just parsedChar -> parsedChar : parseSource xs
    Nothing         -> parseSource xs
    where
        maybeParsedChar = case x of
            '>' -> Just MoveRight
            '<' -> Just MoveLeft
            '+' -> Just Increment
            '-' -> Just Decrement
            '.' -> Just Output
            ',' -> Just Input
            '[' -> Just JumpForward
            ']' -> Just JumpBack
            _   -> Nothing


main = do
    let prog = parseSource "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let tape = ListTape [] (head prog) (tail prog)
    executeCode (Just tape) initialState


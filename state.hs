{-# LANGUAGE InstanceSigs #-}
import           Control.Monad


data TurnstileState = Locked | Unlocked deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)

coin _ = (Thank, Unlocked)

push Unlocked = (Open, Locked)
push Locked   = (Tut, Locked)

monday, tuesday ::TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
        (a3, s3) = push s2
    in ([a1, a2, a3], s2)


regularPerson, hastyPerson, distractedPerson ::TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let (a1, s1) = coin s0
        (a2, s2) = push s1
    in ([a1, a2], s2)

hastyPerson s0 = case push s0 of
    (Open, s1) -> ([Open], s1)
    (a1, s1)   -> let (a2, s2) = coin s1
                      (a3, s3) = push s2
                      in ([a1, a2, a3], s3)


distractedPerson s0 = let (a1, s1) = coin s0 in ([a1], s1)

tuesday s0 =
    let (a1, s1) = regularPerson s0
        (a2, s2) = hastyPerson s1
        (a3, s3) = distractedPerson s2
        (a4, s4) = hastyPerson s3
    in (a1 ++ a2 ++ a3 ++ a4, s4)

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push


instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap
instance Monad (State s) where
    return :: a -> State s a
    return x = State (\s -> (x, s))

    (>>=) :: State s a -> (a -> State s b) -> State s b
    State f >>= g = State h where
        h s1 = (a3, s3) where
               (a2, s2) = f s1
               (a3, s3) = runState (g a2) s2


main = do {
    print $ tuesday Locked;
    print $ tuesday Unlocked
}

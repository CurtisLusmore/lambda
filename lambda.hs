import Control.Monad.State

type Atom = String

data Form = Atom Atom
          | Lambda Atom Form
          | Apply Form Form

instance Show Form where
    show (Atom a) = a
    show (Lambda x y) = "\\" ++ x ++ " . " ++ show y
    show (Apply f x) = "(" ++ show f ++ " " ++ show x ++ ")"

type Env = [(Atom, Form)]
type Prog = State Env
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

data Error = SyntaxError String
           | UnknownAtom Atom
           | FatalError String
           deriving (Show)

type Failable = Either Error

evalAtom :: Atom -> Prog (Failable Form)
evalAtom a = state $ \e -> (reference a e, e) where
    reference a e = case lookup a e of
        Nothing -> Left $ UnknownAtom a
        Just x  -> Right x

evalLambda :: Atom -> Form -> Prog (Failable Form)
evalLambda x y = return $ Right $ Lambda x y

applyLambda :: Form -> Form -> Prog (Failable Form)
applyLambda (Lambda x' y') x = state $ \e ->
    let exec f e = fst $ (return $ Right f) `runState` e
        e' = (x',y'):e
    in (exec x e', e)

evalApply :: Form -> Form -> Prog (Failable Form)
evalApply f x = do
    f' <- eval f
    case f' of
        Left e    -> return $ Left e
        Right f'' -> applyLambda f'' x

eval :: Form -> Prog (Failable Form)
eval (Atom a)     = evalAtom a
eval (Lambda x y) = evalLambda x y
eval (Apply f x)  = evalApply f x
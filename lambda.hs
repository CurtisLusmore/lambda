import Control.Monad.State
import System.IO (hFlush, stdout)
import Text.Parsec hiding (State)

type Atom = String

data Form = Atom Atom
          | Lambda Atom Form
          | Apply Form Form
          | Assign Atom Form

instance Show Form where
    show (Atom a) = a
    show (Lambda x y) = "\\" ++ x ++ " . " ++ show y
    show (Apply f x) = "(" ++ show f ++ " " ++ show x ++ ")"

type Env = [(Atom, Form)]

type Prog = State Env

data Error = SyntaxError ParseError
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

betaReduce :: Atom -> Form -> Form -> Form
betaReduce b b' (Atom a)
  | a == b    = b'
  | otherwise = Atom a
betaReduce b b' (Lambda x y)
  | b == x    = Lambda x y
  | otherwise = Lambda x $ betaReduce b b' y
betaReduce b b' (Apply f x) = Apply (betaReduce b b' f) (betaReduce b b' x)

applyLambda :: Form -> Form -> Prog (Failable Form)
applyLambda (Lambda x y) x' = eval $ betaReduce x x' y

evalApply :: Form -> Form -> Prog (Failable Form)
evalApply f x = do
    f' <- eval f
    case f' of
        Left e    -> return $ Left e
        Right f'' -> applyLambda f'' x

evalAssign :: Atom -> Form -> Prog (Failable Form)
evalAssign x y = state (\e -> (Right y, (x, y):e))

eval :: Form -> Prog (Failable Form)
eval (Atom a)     = evalAtom a
eval (Lambda x y) = evalLambda x y
eval (Apply f x)  = evalApply f x
eval (Assign x y) = evalAssign x y

type Parser = Parsec String ()

symbol :: Parser Char
symbol = oneOf "`~!@#$%^&*-_+|;:',/?[]<>"

identifier :: Parser String
identifier = many1 $ letter <|> symbol <|> digit

parseAtom :: Parser Form
parseAtom = liftM Atom $ identifier

parseLambda :: Parser Form
parseLambda = do
    char '\\'
    xs <- identifier `endBy1` spaces
    char '.'
    spaces
    y <- parseForm
    return $ curry xs y where
        curry (x:xs) y = Lambda x $ curry xs y
        curry [] y     = y

parseApply :: Parser Form
parseApply = do
    fs <- parseTerm `sepBy1` spaces
    return $ foldl1 Apply fs

parseAssign :: Parser Form
parseAssign = do
    x <- identifier
    spaces
    char '='
    spaces
    y <- parseForm
    return $ Assign x y

parseParen :: Parser Form
parseParen = between (char '(') (char ')') parseForm

parseTerm :: Parser Form
parseTerm = parseAtom
        <|> parseLambda
        <|> parseParen

parseForm :: Parser Form
parseForm = try parseAssign
        <|> parseApply

readForm :: String -> Failable Form
readForm input = case parse parseForm "lambda" input of
    Left e -> Left $ SyntaxError e
    Right f -> Right f

exec :: String -> Env -> IO Env
exec line env = case readForm line of
    Left (SyntaxError e) -> do
        putStrLn $ show e
        return env
    Right f -> do
        let (res, env') = eval f `runState` env
        case res of
            Left e -> putStrLn $ show e
            Right f -> putStrLn $ show f
        return env'

main :: IO ()
main = do
    repl [] where
        repl env = do
            prompt "> "
            line <- getLine
            env' <- exec line env
            repl env'
        prompt s = do
            putStr s
            hFlush stdout
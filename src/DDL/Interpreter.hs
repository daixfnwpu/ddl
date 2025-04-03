module DDL.Interpreter where
import qualified Data.Map as Map
import DDL.DDLToken (Expr (..), Pattern (PatternNumber, PatternWildcard, PatternVar), tokenize, parseWithError, exprParser)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

data Function = Function [String] Expr  -- 修正 Function 结构

type Env = Map.Map String (Either Expr Function)

data EvalError
    = UndefinedVariable String
    | TypeError String
    | DivisionByZero
    | InvalidFunctionCall String
    | RuntimeError String
    deriving (Show, Eq)

-- Evaluating Expressions with Error Handling
eval :: Env -> Expr -> Either EvalError Expr
eval env (NumberExpr n) = Right (NumberExpr n)
eval env (IdentifierExpr var) = case Map.lookup var env of
    Just (Left value) -> Right value
    _ -> Left (UndefinedVariable var)

eval env (BinaryOpExpr "+" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a + b))
eval env (BinaryOpExpr "-" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a - b))
eval env (BinaryOpExpr "*" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a * b))
eval env (BinaryOpExpr "/" (NumberExpr _) (NumberExpr 0)) = Left DivisionByZero
eval env (BinaryOpExpr "/" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a / b))
eval env (BinaryOpExpr op _ _) = Left (RuntimeError ("Unsupported operation: " ++ op))

-- Evaluating Functions
eval env (FunctionExpr params body) = Right (FunctionExpr params body)
eval env (CallExpr funcName args) = do
    func <- case Map.lookup funcName env of
        Just (Right (Function params body)) -> Right (params, body)
        _ -> Left (InvalidFunctionCall ("Function not found: " ++ funcName))
    let (params, body) = func
    argValues <- mapM (eval env) args
    let newEnv = Map.union (Map.fromList (zip params (map Left argValues))) env
    eval newEnv body

-- Handle Let Expressions
eval env (LetExpr var valueExpr bodyExpr) = do
    value <- eval env valueExpr
    eval (Map.insert var (Left value) env) bodyExpr

-- Handle If Expressions
eval env (IfExpr cond thenExpr elseExpr) = do
    condResult <- eval env cond
    case condResult of
        NumberExpr 0 -> eval env elseExpr
        _ -> eval env thenExpr
eval env (WhileExpr cond bodyExpr) = evalWhileLoop env cond bodyExpr
eval env (MatchExpr expr cases) = do
    val <- eval env expr
    evalMatchCases env val cases

-- Handle While Expressions
evalWhileLoop :: Env -> Expr -> Expr -> Either EvalError Expr
evalWhileLoop env cond body = do
    condResult <- eval env cond
    case condResult of
        NumberExpr 0 -> Right (NumberExpr 0)
        _ -> do
            _ <- eval env body
            evalWhileLoop env cond body



-- Handle Pattern Matching Expressions
evalMatchCases :: Env -> Expr -> [(Pattern, Expr)] -> Either EvalError Expr
evalMatchCases _ _ [] = Left (RuntimeError "Match expression failed")
evalMatchCases env val ((pat, expr):cases) =
    if matchPattern val pat then eval env expr
    else evalMatchCases env val cases



-- Pattern Matching Helper Function
matchPattern :: Expr -> Pattern -> Bool
matchPattern (NumberExpr n) (PatternNumber n') = n == n'
matchPattern (StringExpr s) (PatternVar s') = s == s'
matchPattern (IdentifierExpr _) PatternWildcard = True
matchPattern _ _ = False

-- REPL Function
repl :: Env -> IO ()
repl env = do
    putStr "DDL> "
    hFlush stdout
    input <- getLine
    if input == ":quit"
        then putStrLn "Bye!"
        else do
            case tokenize input of
                Left err  -> print err
                Right toks -> print toks
            case parseWithError exprParser input of
                Left err -> print err
                Right ast -> do
                    let result = eval env ast
                    case result of
                        Left err -> putStrLn $ "Runtime Error: " ++ show err
                        Right val -> print val
            repl env

testEnv :: Env
testEnv = Map.fromList 
    [ ("x", Left (NumberExpr 10))
    , ("y", Left (NumberExpr 5))
    , ("f", Right (Function ["a"] (BinaryOpExpr "+" (IdentifierExpr "a") (NumberExpr 1))))
    ]

runMain :: IO ()
runMain = do
    print $ eval testEnv (BinaryOpExpr "+" (NumberExpr 3) (NumberExpr 4)) -- 应该返回 Right (NumberExpr 7)
    print $ eval testEnv (IdentifierExpr "x") -- 应该返回 Right (NumberExpr 10)
    print $ eval testEnv (BinaryOpExpr "*" (IdentifierExpr "x") (IdentifierExpr "y")) -- 应该返回 Right (NumberExpr 50)
    print $ eval testEnv (CallExpr "f" [NumberExpr 4]) -- 应该返回 Right (NumberExpr 5)
    print $ eval testEnv (IdentifierExpr "z") -- 应该返回 Left (UndefinedVariable "z")
    print $ eval testEnv (BinaryOpExpr "/" (NumberExpr 1) (NumberExpr 0)) -- 应该返回 Left DivisionByZero
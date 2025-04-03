module DDL.Interpreter where

import qualified Data.Map as Map
import DDL.DDLToken (Expr (..), Pattern (PatternNumber, PatternWildcard, PatternVar), tokenize, parseWithError, exprParser)
import GHC.IO.Handle (hFlush)
import GHC.IO.FD (stdout)

type Env = Map.Map String Expr

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
    Just value -> Right value
    Nothing -> Left (UndefinedVariable var)

eval env (BinaryOpExpr "+" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a + b))
eval env (BinaryOpExpr "-" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a - b))
eval env (BinaryOpExpr "*" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a * b))
eval env (BinaryOpExpr "/" (NumberExpr _ ) (NumberExpr 0)) = Left DivisionByZero
eval env (BinaryOpExpr "/" (NumberExpr a) (NumberExpr b)) = Right (NumberExpr (a / b))
eval env (BinaryOpExpr op _ _) = Left (RuntimeError ("Unsupported operation: " ++ op))

-- Evaluating Functions
eval env (FunctionExpr params body) = Right (FunctionExpr params body)
eval env (CallExpr funcName args) = do
    func <- case Map.lookup funcName env of
        Just (Right (FunctionExpr params body)) -> Right (params, body)
        _ -> Left (RuntimeError ("Function not found: " ++ funcName))
    let (params, body) = func
    argValues <- mapM (eval env) args
    let newEnv = Map.fromList (zip params argValues) `Map.union` env
    eval newEnv body

-- Handle IdentifierExpr (i.e., variables)
eval env (IdentifierExpr var) =
    case Map.lookup var env of
        Just value -> Right value
        Nothing -> Left $ "Undefined variable: " ++ var

-- Handle Binary Operations (e.g., addition, subtraction)
eval env (BinaryOpExpr op lhs rhs) = do
    lhs' <- eval env lhs
    rhs' <- eval env rhs
    evalBinaryOp op lhs' rhs'

-- Handle LetExpr (variable assignments)
eval env (LetExpr var valueExpr bodyExpr) = do
    value <- eval env valueExpr
    eval (Map.insert var value env) bodyExpr

-- Handle If Expressions
eval env (IfExpr cond thenExpr elseExpr) = do
    condResult <- eval env cond
    case condResult of
        NumberExpr 0 -> eval env elseExpr
        _ -> eval env thenExpr

-- Handle While Expressions (basic loop)
eval env (WhileExpr cond bodyExpr) = evalWhileLoop env cond bodyExpr

-- Handle Pattern Matching Expressions (MatchExpr)
eval env (MatchExpr expr cases) = do
    val <- eval env expr
    evalMatchCases val cases

-- Handle function calls (simple)
eval env (CallExpr funcName args) = 
    -- Function evaluation logic here
    Left "Function calls not implemented yet"


evalBinaryOp :: String -> Expr -> Expr -> Either String Expr
evalBinaryOp "+" (NumberExpr a) (NumberExpr b) = Right (NumberExpr (a + b))
evalBinaryOp "-" (NumberExpr a) (NumberExpr b) = Right (NumberExpr (a - b))
evalBinaryOp "*" (NumberExpr a) (NumberExpr b) = Right (NumberExpr (a * b))
evalBinaryOp "/" (NumberExpr a) (NumberExpr b) = 
    if b == 0 then Left "Division by zero"
    else Right (NumberExpr (a / b))
evalBinaryOp op _ _ = Left $ "Unsupported operation: " ++ op

evalWhileLoop :: Env -> Expr -> Expr -> Either String Expr
evalWhileLoop env cond body = do
    condResult <- eval env cond
    case condResult of
        NumberExpr 0 -> Right (NumberExpr 0)  -- Exit condition
        _ -> do
            eval env body
            evalWhileLoop env cond body  -- Repeat loop


evalMatchCases :: Env -> Expr -> [(Pattern, Expr)] -> Either String Expr
evalMatchCases _ _ [] = Left "Match expression failed"
evalMatchCases env val ((pat, expr):cases) = do
    if matchPattern val pat then eval env expr
    else evalMatchCases  env val cases


matchPattern :: Expr -> Pattern -> Bool
matchPattern (NumberExpr n) (PatternNumber n') = n == n'
matchPattern (StringExpr s) (PatternVar s') = s == s'
matchPattern (IdentifierExpr _) PatternWildcard = True
matchPattern _ _ = False


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
                        Left err -> putStrLn $ "Runtime Error: " ++ err
                        Right val -> print val
            repl env

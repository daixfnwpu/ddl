{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module DDL.DDLToken where
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error
    ( ParseError)
import Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isLetter)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Text.Printf (printf)
--import Text.Parsec.Indent 

data Token
    = TokKeyword String     -- 关键字
    | TokIdentifier String  -- 标识符
    | TokNumber Double      -- 数字（支持浮点数）
    | TokString String      -- 字符串（支持多行）
    | TokBool Bool          -- 布尔值（true/false）
    | TokNull               -- 空值（null）
    | TokOperator String    -- 运算符
    | TokSymbol String      -- 符号（括号、逗号等）
    | TokComment String     -- 注释
    | TokEOF                -- 结束
    deriving (Show, Eq, Ord)

-- 定义 AST 结构
data Expr
    = NumberExpr Double
    | StringExpr String
    | IdentifierExpr String
    | BinaryOpExpr String Expr Expr
    | CallExpr String [Expr]
    | IfExpr Expr Expr Expr
    | LetExpr String Expr Expr
    | MatchExpr Expr [(Pattern,Expr)]
    | FunctionDef String [Pattern] Expr
    | TypeAnnotation String String
    | WhileExpr Expr Expr               -- while 语句
    | ForExpr String Expr Expr          -- for 语句
    | TryCatchExpr Expr String Expr     -- try-catch 语句
    | FunctionExpr [String] Expr
    deriving (Show, Eq)

data Pattern
    = PatternVar String
    | PatternNumber Double
    | PatternWildcard
    deriving (Show,Eq)
-- 关键字列表
keywords :: [String]
keywords = ["if", "else", "match", "while", "for", "fun", "let", "return", "try", "catch"]

-- TokenParseError with position information
data TokenParseError = TokenParseError ParseError  -- You may want to store the whole `ParseError` here
    deriving Show

-- MessageParseError with a custom message
data MessageParseError = MessageParseError String
    deriving Show

-- A wrapper for both error types
data ParseErrorWrapper
    = TokenError TokenParseError
    | MessageError MessageParseError
    deriving Show

-- 格式化 ParseError 为自定义 MessageParseError
convertToMessageError :: ParseError -> MessageParseError
convertToMessageError err =
    let pos = errorPos err
        line = sourceLine pos
        col = sourceColumn pos
        msg = "Parsing error: " ++ show err
    in MessageParseError $ printf msg line col



type Env = Map.Map String Expr

stdLib :: Env
stdLib = Map.fromList
    [ ("sqrt", FunctionDef "sqrt" [PatternVar "x"] (CallExpr "sqrt" [IdentifierExpr "x"]))
    , ("sin", FunctionDef "sin" [PatternVar "x"] (CallExpr "sin" [IdentifierExpr "x"]))
    , ("cos", FunctionDef "cos" [PatternVar "x"] (CallExpr "cos" [IdentifierExpr "x"]))
    , ("tan", FunctionDef "tan" [PatternVar "x"] (CallExpr "tan" [IdentifierExpr "x"]))
    , ("length", FunctionDef "length" [PatternVar "s"] (CallExpr "length" [IdentifierExpr "s"]))
    , ("toUpper", FunctionDef "toUpper" [PatternVar "s"] (CallExpr "toUpper" [IdentifierExpr "s"]))
    , ("map", FunctionDef "map" [PatternVar "f", PatternVar "lst"] (CallExpr "map" [IdentifierExpr "f", IdentifierExpr "lst"]))
    , ("filter", FunctionDef "filter" [PatternVar "f", PatternVar "lst"] (CallExpr "filter" [IdentifierExpr "f", IdentifierExpr "lst"]))
    , ("reduce", FunctionDef "reduce" [PatternVar "f", PatternVar "acc", PatternVar "lst"] (CallExpr "reduce" [IdentifierExpr "f", IdentifierExpr "acc", IdentifierExpr "lst"]))
    ]

-- 格式化错误信息，提供具体的行号和列号
-- Format a ParseError with line and column information
formatParseError :: ParseError -> String
formatParseError err =
    let pos = errorPos err  -- Extract the position from the ParseError 
        line = sourceLine pos
        col  = sourceColumn pos 
    in printf "Parse error at line %d, column %d: %s" line col (show err)



-- 改进错误报告
parseWithError :: Parser a -> String -> Either String a
parseWithError p input = case parse p "Parser" input of
    Left err -> Left ("Syntax Error: " ++ show err)
    Right res -> Right res


-- 解析关键字
keywordParser :: Parser Token
keywordParser = do
    kw <- choice (map (try . string) keywords)
    notFollowedBy alphaNum
    spaces
    return (TokKeyword kw)

-- 解析 Unicode 标识符（变量、函数名）
identifierParser :: Parser Token
identifierParser = do
    first <- satisfy isLetter <|> char '_'
    rest <- many (satisfy (\x -> isLetter x || isAlphaNum x || x == '_'))
    spaces
    let name = first : rest
    return $ if name `elem` keywords then TokKeyword name else TokIdentifier name

numberParser :: Parser Token
numberParser = do
    sign <- optionMaybe (char '+' <|> char '-')  -- 处理正负号
    num <- many1 digit
    optFraction <- optionMaybe (char '.' >> many1 digit)
    optExponent <- optionMaybe (oneOf "eE" >> optionMaybe (char '+' <|> char '-') >>= \es -> many1 digit >>= \expPart -> return (es, expPart))

    let numStr = case optFraction of
            Just frac -> num ++ "." ++ frac
            Nothing   -> num
        expStr = case optExponent of
            Just (es, expPart) -> "e" ++ maybe "" (:[]) es ++ expPart
            Nothing            -> ""
        fullStr = maybe "" (:[]) sign ++ numStr ++ expStr

    spaces
    return $ TokNumber (read fullStr)

-- 解析字符串（支持多行 UTF-8）
stringParser :: Parser Token
stringParser = do
    char '"'
    str <- manyTill (try (string "\\\n") *> return '\n' <|> anyChar) (try (char '"'))
    spaces
    return $ TokString str

-- 解析运算符
operators :: [String]
operators = ["+", "-", "*", "/", "=", "==", "!=", "<", ">", "<=", ">=","|","||"]

operatorParser :: Parser Token
operatorParser = choice (map (try . string) operators) >>= \op -> spaces >> return (TokOperator op)

-- 解析 while 语句
whileExprParser :: Parser Expr
whileExprParser = do
    _ <- string "while"
    spaces
    cond <- exprParser
    spaces
    _ <- string "do"
    spaces
    body <- exprParser
    return (WhileExpr cond body)

-- 解析 for 语句
forExprParser :: Parser Expr
forExprParser = do
    _ <- string "for"
    spaces
    var <- many1 letter
    spaces
    _ <- string "in"
    spaces
    rangeExpr <- exprParser
    spaces
    _ <- string "do"
    spaces
    body <- exprParser
    return (ForExpr var rangeExpr body)

-- 解析 try-catch 语句
tryCatchParser :: Parser Expr
tryCatchParser = do
    _ <- string "try"
    spaces
    tryExpr <- exprParser
    spaces
    _ <- string "catch"
    spaces
    var <- many1 letter
    spaces
    _ <- string "->"
    spaces
    catchExpr <- exprParser
    return (TryCatchExpr tryExpr var catchExpr)


-- 解析符号
symbols :: [Char]
symbols = "(){};,>="

symbolParser :: Parser Token
symbolParser = oneOf symbols >>= \sym -> spaces >> return (TokSymbol [sym])

-- 解析注释（单行和多行）
commentParser :: Parser ()
commentParser = do
    _ <- try (string "//" >> manyTill anyChar newline)
    spaces
    return ()
  <|> do
    _ <- try (string "/*" >> manyTill anyChar (try (string "*/")))
    spaces
    return ()

-- 解析换行符（可选）
newlineParser :: Parser ()
newlineParser = void newline <|> void (string "\r\n")

-- 解析模式匹配;
patternParser :: Parser Pattern
patternParser = try (do
                        TokNumber n <- numberParser
                        return (PatternNumber n))
                <|> (PatternVar <$> many1 letter)
                <|> (char '_' >> return PatternWildcard)

-- 解析所有 Token
tokenParser :: Parser [Token]
tokenParser = do
    spaces
    manyTill (choice [
            try keywordParser
        <|> try identifierParser
        <|> try numberParser
        <|> try stringParser
        <|> try operatorParser
        <|> try symbolParser
        <|> try (commentParser >> return TokEOF)
        <|> try (newlineParser >> return TokEOF)
        ]) eof

-- 解析表达式（AST 解析器）
exprParser :: Parser Expr
exprParser = try matchExprParser <|> try functionDefParser <|> try typeAnnotationParser <|> try ifExprParser
                <|> try letExprParser <|> try binaryOpParser <|> try callParser <|> simpleExpr
                <|> try whileExprParser
                <|> try forExprParser
                <|> try tryCatchParser


simpleExpr :: Parser Expr
simpleExpr = numberExprParser <|> stringExprParser <|> identifierExprParser

numberExprParser :: Parser Expr
numberExprParser = do
    TokNumber n <- numberParser
    return (NumberExpr n)

stringExprParser :: Parser Expr
stringExprParser = do
    TokString s <- stringParser
    return (StringExpr s)

identifierExprParser :: Parser Expr
identifierExprParser = do
    TokIdentifier s <- identifierParser
    return (IdentifierExpr s)

binaryOpParser :: Parser Expr
binaryOpParser = do
    lhs <- simpleExpr
    TokOperator op <- operatorParser
    BinaryOpExpr op lhs <$> exprParser

callParser :: Parser Expr
callParser = do
    TokIdentifier name <- identifierParser
    _ <- char '('
    args <- exprParser `sepBy` char ','
    _ <- char ')'
    return (CallExpr name args)

-- 解析 if 语句
ifExprParser :: Parser Expr
ifExprParser = do
    _ <- string "if"
    spaces
    cond <- exprParser
    spaces
    _ <- string "then"
    spaces
    thenBranch <- exprParser
    spaces
    _ <- string "else"
    spaces
    elseBranch <- exprParser
    return (IfExpr cond thenBranch elseBranch)

-- 解析 let 绑定
letExprParser :: Parser Expr
letExprParser = do
    _ <- string "let"
    spaces
    TokIdentifier var <- identifierParser
    spaces
    _ <- char '='
    spaces
    value <- exprParser
    spaces
    _ <- string "in"
  --  indented
    body <- exprParser
    return (LetExpr var value body)

-- 解析 match 语句
matchExprParser :: Parser Expr
matchExprParser = do
    _ <- string "match"
    spaces
    expr <- exprParser
    spaces
    _ <- char '{'
    spaces
    cases <- many matchCaseParser
    _ <- char '}'
    return (MatchExpr expr cases)

matchCaseParser :: Parser (Pattern, Expr)
matchCaseParser = do
    pat <- patternParser
    spaces
    _ <- string "=>"
    spaces
    expr <- exprParser
    spaces
    return (pat, expr)

-- 解析函数定义
functionDefParser :: Parser Expr
functionDefParser = do
    _ <- string "fun"
    spaces
    name <- many1 letter
    spaces
    args <- many patternParser
    spaces
    _ <- char '='
    spaces
    body <- exprParser
    return (FunctionDef name args body)

-- 解析类型声明
typeAnnotationParser :: Parser Expr
typeAnnotationParser = do
    name <- many1 letter
    spaces
    _ <- string "::"
    spaces
    typeName <- many1 letter
    return (TypeAnnotation name typeName)


-- 交互式 REPL
tokenize :: String -> Either ParseError [Token]
tokenize = parse tokenParser "Lexer"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse exprParser "Parser"

getContentsUntilEmpty :: IO String
getContentsUntilEmpty = do
    line <- getLine
    if null line then return "" else do
        rest <- getContentsUntilEmpty
        return (line ++ "\n" ++ rest)

repl :: IO ()
repl = do
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
                Right ast -> print ast
            repl

runMain :: IO ()
runMain = do
    putStrLn "Welcome to the DDL REPL. Type :quit to exit."
    repl

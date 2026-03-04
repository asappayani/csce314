module HW_Expr
  ( Op(..)
  , Expr(..)
  , Token(..)
  , tokenize
  , parseExprString
  , run
  , showPrefix
  , toPrefix
  , eval
  ) where

-- CSCE 314 — HW: Parse & Evaluate Arithmetic (Prefix AST)
-- Starter file for students.
-- Replace each `undefined` with your implementation and remove TODOs as you complete them.
-- You may ONLY use Prelude and Data.Char (no external parsing libraries).

import Data.Char (isDigit, isSpace)

-- -------------------------------------------------------------------------
-- Operators and AST (Given)
-- -------------------------------------------------------------------------

data Op = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Expr
  = Lit Integer
  | Bin Op Expr Expr
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- Tokens (Given)
-- -------------------------------------------------------------------------

data Token
  = TPlus | TMinus | TMul | TDiv
  | TLParen | TRParen
  | TInt Integer
  deriving (Eq, Show)

-- -------------------------------------------------------------------------
-- 1) TOKENIZER
--    TODO: Implement 'tokenize' that turns a String into [Token].
--    - Ignore whitespace
--    - Support multi-digit integers
--    - Recognize + - * / ( )
--    - You may use: isDigit, isSpace
--    - (Optional) You can choose to throw an error on unknown characters
--      or extend the assignment to return Either for tokenizer errors.
-- -------------------------------------------------------------------------

tokenize :: String -> [Token]
tokenize s = tokenizeNoSpace [x | x <- s, not (isSpace x)]
  where
    tokenizeNoSpace [] = []
    tokenizeNoSpace (x:xs)
      | x == '+' = TPlus : tokenizeNoSpace xs
      | x == '-' = TMinus : tokenizeNoSpace xs
      | x == '*' = TMul : tokenizeNoSpace xs
      | x == '/' = TDiv : tokenizeNoSpace xs
      | x == '(' = TLParen : tokenizeNoSpace xs
      | x == ')' = TRParen : tokenizeNoSpace xs
      | isDigit x =
          let (digit, rest) = takeDigits (x:xs)
          in TInt (read digit) : tokenizeNoSpace rest
      | otherwise = error ("Unknown character: " ++ [x])

    takeDigits [] = ([], [])
    takeDigits (d:ds)
      | isDigit d =
          let (digits, rest) = takeDigits ds
          in (d:digits, rest)
      | otherwise = ([], d:ds)

-- -------------------------------------------------------------------------
-- 2) PARSER (Recursive Descent)
-- Grammar:
--   Expr   ::= Term (('+' | '-') Term)*
--   Term   ::= Factor (('*' | '/') Factor)*
--   Factor ::= INT | '(' Expr ')'
-- Return type pattern for nonterminals:
--   parseX :: [Token] -> Either String (Expr, [Token])
-- Notes:
--   - Use recursion
--   - Propagate errors via Left "message"
-- -------------------------------------------------------------------------

parseExprString :: String -> Either String Expr
parseExprString s =
  case parseExpr (tokenize s) of
    Left msg -> Left msg
    Right (okTree, []) -> Right okTree
    Right (_, extraBits) -> Left ("Unexpected token at end: " ++ show extraBits)

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr toks = do
  (firstThing, restStuff) <- parseTerm toks
  loop firstThing restStuff
  where
    loop soFar (TPlus:more) = do
      (nextThing, r2) <- parseTerm more
      loop (Bin Add soFar nextThing) r2
    loop soFar (TMinus:more) = do
      (nextThing, r2) <- parseTerm more
      loop (Bin Sub soFar nextThing) r2
    loop soFar whateverLeft = Right (soFar, whateverLeft)

parseTerm :: [Token] -> Either String (Expr, [Token])
parseTerm toks = do
  (firstPiece, restStuff) <- parseFactor toks
  loop2 firstPiece restStuff
  where
    loop2 soFar (TMul:more) = do
      (nextPiece, r2) <- parseFactor more
      loop2 (Bin Mul soFar nextPiece) r2
    loop2 soFar (TDiv:more) = do
      (nextPiece, r2) <- parseFactor more
      loop2 (Bin Div soFar nextPiece) r2
    loop2 soFar whateverLeft = Right (soFar, whateverLeft)

parseFactor :: [Token] -> Either String (Expr, [Token])
parseFactor (TInt n : xs) = Right (Lit n, xs)
parseFactor (TLParen : xs) = do
  (inside, rest) <- parseExpr xs
  case rest of
    (TRParen : ys) -> Right (inside, ys)
    _ -> Left "Unclosed parenthesis"
parseFactor [] = Left "Expected factor"
parseFactor (weird:_) = Left ("Expected factor, got " ++ show weird)

-- -------------------------------------------------------------------------
-- 3) PRETTY-PRINT (Prefix)
--   toPrefix (Lit 2) == "2"
--   toPrefix (Bin Add (Lit 2) (Lit 3)) == "add 2 3"
--   Wrap nested subexpressions in parentheses for readability:
--   toPrefix (Bin Add (Bin Mul (Lit 2) (Lit 5)) (Lit 9))
--     == "add (mult 2 5) 9"
-- -------------------------------------------------------------------------

toPrefix :: Expr -> String
toPrefix (Lit n) = show n
toPrefix (Bin op a b) =
  unwords [part | part <- [opWord op, showSide a, showSide b]]
  where
    opWord Add = "add"
    opWord Sub = "sub"
    opWord Mul = "mult"
    opWord Div = "div"

    showSide (Bin op2 l r) = "(" ++ toPrefix (Bin op2 l r) ++ ")"
    showSide e = toPrefix e

-- -------------------------------------------------------------------------
-- 4) EVALUATION (Safe with Either)
--   eval (Lit 2) == Right 2
--   eval (Bin Div (Lit 8) (Lit 0)) == Left "Division by zero"
--   Use recursion and pattern matching
-- -------------------------------------------------------------------------

eval :: Expr -> Either String Integer
eval (Lit n) = Right n
eval (Bin op leftStuff rightStuff) = do
  lval <- eval leftStuff
  rval <- eval rightStuff
  case op of
    Add -> Right (lval + rval)
    Sub -> Right (lval - rval)
    Mul -> Right (lval * rval)
    Div ->
      if rval == 0
        then Left "Division by zero"
        else Right (lval `div` rval)

showPrefix :: String -> Either String String
showPrefix raw = do
  tree <- parseExprString raw
  Right (toPrefix tree)

run :: String -> Either String Integer
run raw = do
  tree <- parseExprString raw
  eval tree

-- -------------------------------------------------------------------------
-- End of Starter
-- -------------------------------------------------------------------------

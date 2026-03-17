module Week7Code_Along where
-- Week 7 Code-Along (CSCE 314)
-- Topic: Trees in FP -> Haskell (binary trees, folds, traversals, Functor/Foldable, expression trees)

-- Load in GHCi:  :load Week7Code_Along.hs

import Data.Monoid (Sum(..))

--------------------------------------------------------------------------------
-- 1) A simple binary tree (values at leaves; nodes only connect subtrees)
--------------------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- Map over trees (alias to fmap; we also supply a Functor instance below)
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a)   = Leaf (f a)
mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)

-- Fold over trees (catamorphism for our leaf-labeled tree)
-- leafCase handles a leaf's value; nodeCase combines the two subtree results.
foldTree :: (a -> r) -> (r -> r -> r) -> Tree a -> r
foldTree leafCase _       (Leaf a)   = leafCase a
foldTree leafCase nodeCase (Node l r) = nodeCase (foldTree leafCase nodeCase l)
                                                (foldTree leafCase nodeCase r)

-- Standard computations via fold
numLeaves :: Tree a -> Int
numLeaves = foldTree (const 1) (+)

height :: Tree a -> Int
height = foldTree (const 1) (\l r -> 1 + max l r)

leaves :: Tree a -> [a]
leaves = foldTree (:[]) (++)

-- Build a (roughly) balanced tree from a non-empty list by splitting halves.
fromListBalanced :: [a] -> Tree a
fromListBalanced []  = error "fromListBalanced: empty list"
fromListBalanced [x] = Leaf x
fromListBalanced xs  =
  let n      = length xs
      mid    = n `div` 2
      (ls, rs) = splitAt mid xs
  in Node (fromListBalanced ls) (fromListBalanced rs)

-- Instances: Functor (map) and Foldable (generic reductions like sum, toList)
instance Functor Tree where
  fmap = mapTree

instance Foldable Tree where
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l <> foldMap f r

-- Inorder traversal (leaf order) equals 'toList' from Foldable
inorder :: Tree a -> [a]
inorder = leaves

--------------------------------------------------------------------------------
-- 2) Expression trees (operators at nodes, values at leaves)
--------------------------------------------------------------------------------

data Op = Add | Sub | Mul | Div | Pow
  deriving (Eq, Show)

data Expr
  = Val Integer
  | Op2 Op Expr Expr
  deriving (Eq, Show)

-- A fold for expression trees
foldExpr :: (Integer -> r) -> (Op -> r -> r -> r) -> Expr -> r
foldExpr f _ (Val n) = f n
foldExpr f g (Op2 op a b) = g op (foldExpr f g a) (foldExpr f g b)

-- Pretty-printers (prefix, infix, postfix). Always parenthesize in infix for clarity.
opName :: Op -> String
opName Add = "add"
opName Sub = "sub"
opName Mul = "mul"
opName Div = "div"
opName Pow = "pow"

opSym :: Op -> String
opSym Add = "+"
opSym Sub = "-"
opSym Mul = "*"
opSym Div = "/"
opSym Pow = "^"

prettyPrefix :: Expr -> String
prettyPrefix = foldExpr show (\op x y -> opName op ++ " " ++ wrap x ++ " " ++ wrap y)
  where
    wrap s = if ' ' `elem` s || '(' `elem` s then "(" ++ s ++ ")" else s

prettyPostfix :: Expr -> String
prettyPostfix = foldExpr show (\op x y -> wrap x ++ " " ++ wrap y ++ " " ++ opName op)
  where
    wrap s = if ' ' `elem` s || '(' `elem` s then "(" ++ s ++ ")" else s

prettyInfix :: Expr -> String
prettyInfix = foldExpr show (\op x y -> "(" ++ x ++ " " ++ opSym op ++ " " ++ y ++ ")")

-- Safe evaluator with Either
evalExpr :: Expr -> Either String Integer
evalExpr = foldExpr Right combine
  where
    combine :: Op -> Either String Integer -> Either String Integer -> Either String Integer
    combine op ex ey = do
      x <- ex
      y <- ey
      case op of
        Add -> Right (x + y)
        Sub -> Right (x - y)
        Mul -> Right (x * y)
        Div -> if y == 0 then Left "Division by zero" else Right (x `div` y)
        Pow -> if y < 0 then Left "Negative exponent not supported"
                        else Right (x ^ y)

-- Example: 5 * 7^2 + 2 / 4 * 8 - 1
-- Build it by hand: ((5 * (7 ^ 2)) + ((2 / 4) * 8)) - 1
exprExample :: Expr
exprExample =
  let term1 = Op2 Mul (Val 5) (Op2 Pow (Val 7) (Val 2))
      term2 = Op2 Mul (Op2 Div (Val 2) (Val 4)) (Val 8)
      sum12 = Op2 Add term1 term2
  in Op2 Sub sum12 (Val 1)

--------------------------------------------------------------------------------
-- 3) Small demo helpers for GHCi
--------------------------------------------------------------------------------

-- Make a small balanced tree for demos
t7 :: Tree Int
t7 = fromListBalanced [1..7]

tn :: Int -> Tree Int
tn n = fromListBalanced [1..n]

-- Show Functor law sanity checks (for manual exploration in GHCi)
-- fmap id t7 == t7
-- fmap (g . f) t7 == fmap g (fmap f t7)

-- Examples demonstrating Foldable
sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMap Sum

productTree :: Num a => Tree a -> a
productTree = foldr (*) 1

-- A few quick labeled demos for 'main' (for convenience)
demo :: IO ()
demo = do
  putStrLn "== Binary Tree (values at leaves) =="
  putStrLn $ "t7 = fromListBalanced [1..7] -> " ++ show t7
  putStrLn $ "numLeaves t7 = " ++ show (numLeaves t7)
  putStrLn $ "height t7    = " ++ show (height t7)
  putStrLn $ "inorder t7   = " ++ show (inorder t7)
  putStrLn $ "fmap (*2) t7 = " ++ show (fmap (*2) t7)
  putStrLn $ "sumTree t7   = " ++ show (sumTree t7)

  putStrLn ""
  putStrLn "== Expression Tree Example =="
  putStrLn $ "expr_example (infix)   = " ++ prettyInfix exprExample
  putStrLn $ "expr_example (prefix)  = " ++ prettyPrefix exprExample
  putStrLn $ "expr_example (postfix) = " ++ prettyPostfix exprExample
  putStrLn $ "evalExpr expr_example  = " ++ show (evalExpr exprExample)

-- Keep 'main' tiny so you can also explore interactively in GHCi.
main :: IO ()
main = demo

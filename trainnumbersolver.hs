import Data.List
import Control.Monad

data TrainExpr = Leaf Int | OpNode TrainExpr Op TrainExpr

instance Show TrainExpr where
  show (Leaf x) = show x
  show (OpNode lhs op rhs) = "(" ++ show lhs ++ ")" ++ show op ++ "(" ++ show rhs ++ ")"

data Op = Add | Sub | Mul | Div | Exp deriving (Show)

apply :: Op -> Int -> Int -> Maybe Int
apply Add a b = Just $ a + b
apply Sub a b = Just $ a - b
apply Mul a b = Just $ a * b
apply Div a b = if a `mod` b  == 0 then Just $ a `quot` b else Nothing
apply Exp a b = if b >= 0 then Just $ a ^ b else Nothing

buildtree :: (Op, Op, Op) -> (Int, Int, Int, Int) -> TrainExpr
buildtree (lhsOp, midOp, rhsOp) (n1, n2, n3, n4) =
  OpNode (OpNode (Leaf n1) lhsOp (Leaf n2)) midOp (OpNode (Leaf n3) rhsOp (Leaf n4))

solveTree :: TrainExpr -> Maybe Int
solveTree (Leaf x) = Just x
solveTree (OpNode lhs op rhs) = case lhsSolution of
  Just x -> case rhsSolution of
    Just y -> apply op x y
    Nothing -> Nothing
  Nothing -> Nothing
  where
    lhsSolution = solveTree lhs
    rhsSolution = solveTree rhs

solutions :: [Int] -> [TrainExpr]
solutions input = solutionsFromNumCombs (permutations input) (replicateM 3 [Add, Sub, Mul, Div, Exp])

-- recursively solve all combs of nums, adding solutions to the answer
solutionsFromNumCombs :: [[Int]] -> [[Op]] -> [TrainExpr]
solutionsFromNumCombs [] _ = []
solutionsFromNumCombs (curNums:otherNums) ops =
    (solutionsFromOpCombs curNums ops) ++ solutionsFromNumCombs otherNums ops

solutionsFromOpCombs :: [Int] -> [[Op]] -> [TrainExpr]
solutionsFromOpCombs _ [] = []
solutionsFromOpCombs nums (curOps:otherOps) = case solution of
  Just x -> if x == 10 then tree:otherSolutions else otherSolutions
  Nothing -> otherSolutions
  where
    tree = buildtree ((curOps !! 0), (curOps !! 1), (curOps !! 2)) ((nums !! 0), (nums !! 1), (nums !! 2), (nums !! 3))
    solution = solveTree tree
    otherSolutions = solutionsFromOpCombs nums otherOps

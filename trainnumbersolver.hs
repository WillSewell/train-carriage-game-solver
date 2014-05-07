import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.Interpreter

data TrainExpr = Leaf Int | OpNode TrainExpr Op TrainExpr

instance Show TrainExpr where
  show (Leaf x) = show x
  show (OpNode lhs op rhs) = "(" ++ show lhs ++ ")" ++ show op ++ "(" ++ show rhs ++ ")"

data Op = Add | Sub | Mul | Div | Pow deriving (Show)

apply :: Op -> Int -> Int -> Maybe Int
apply Add a b = Just a + b
apply Sub a b = Just a - b
apply Mul a b = Just a * b
apply Div a b = if a `div` b then Just a `quot` b else Nothing
apply Pow a b = Just a ^ b

buildtree :: (Op, Op, Op) -> (Int, Int, Int, Int) -> TrainExpr
buildtree (lhsOp, midOp, rhsOp) (n1, n2, n3, n4) =
  OpNode (OpNode n1 lhsOp n2) midOp (OpNode n3 rhsOp n4)

solveTree :: TrainExpr -> Maybe Int
solveTree (Leaf x) = x
solveTree (OpNode lhs op rhs) = solveTree lhs `op` solveTree rhs

solutions :: [Int] -> [TrainExpr]
solutions input = solutionsFromNumCombs (permutations input) (replicateM 3 [Add, Sub, Mul, Div, Pow])

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

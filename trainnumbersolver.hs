module Main where

import Haste
import Data.List
import Control.Monad

data TrainExpr = Leaf Int | OpNode TrainExpr Op TrainExpr

instance Show TrainExpr where
  show (Leaf x) = show x
  show (OpNode (Leaf x) op (Leaf y)) = show x ++ show op ++ show y
  show (OpNode lhs op rhs) = "(" ++ show lhs ++ ")" ++ show op ++ "(" ++ show rhs ++ ")"

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

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
solveTree (OpNode lhs op rhs) = do
  x <- solveTree lhs
  y <- solveTree rhs
  apply op x y

solutions :: [Int] -> [TrainExpr]
solutions input = solutionsFromNumCombs (permutations input) (replicateM 3 [Add, Sub, Mul, Div, Exp])

solutionsFromNumCombs :: [[Int]] -> [[Op]] -> [TrainExpr]
solutionsFromNumCombs nums ops = concatMap (solutionsFromOpCombs ops) nums

solutionsFromOpCombs :: [[Op]] -> [Int] -> [TrainExpr]
solutionsFromOpCombs ops nums = foldl fn [] ops
  where
    fn acc x =
      let tree = buildtree (tuplify3 x) $ tuplify4 nums
      in case solveTree tree of
       Just x -> if x == 10 then tree:acc else acc
       Nothing -> acc
    

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x1, x2, x3] = (x1, x2, x3)

tuplify4 :: [a] -> (a, a, a, a)
tuplify4 [x1,x2,x3,x4] = (x1, x2, x3, x4)

main = do
  Just submit <- elemById "submit"
  submit `onEvent` OnClick $ \_ _ -> do
    Just input <- elemById "input"
    Just numsRaw <- (getValue input)
    let nums = map (read . (:" ")) numsRaw :: [Int] in do
      Just output <- elemById "output"
      setProp output "innerHTML" $ showSolutions $ solutions nums

showSolutions :: [TrainExpr] -> String
showSolutions = foldl (\acc x -> acc ++ "<p>" ++ show x ++ "</p>") ""

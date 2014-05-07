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

buildtree :: (Char, Char, Char) -> (Int, Int, Int, Int) -> TrainExpr
buildtree (lhsOp, midOp, rhsOp) (n1, n2, n3, n4) =
  OpNode (OpNode n1 lhsOp n2) midOp (OpNode n3 rhsOp n4)

solveTree :: TrainExpr -> Maybe Int
solveTree (Leaf x) = x
solveTree (OpNode lhs op rhs) = solveTree lhs `op` solveTree rhs

--- Next need to build trees from the permutations generated below

solutions :: [Int] -> [[Char]]
solutions input = solutionsFromNumCombs (permutations input) (replicateM 3 ['+', '-', '*', '/'])

-- recursively solve all combs of nums, adding solutions to the answer
solutionsFromNumCombs :: [[Int]] -> [[Char]] -> [[Char]]
solutionsFromNumCombs [] _ = []
solutionsFromNumCombs (curNums:otherNums) ops =
    (solutionsFromOpCombs curNums ops) ++ solutionsFromNumCombs otherNums ops

solutionsFromOpCombs :: [Int] -> [[Char]] -> [[Char]]
solutionsFromOpCombs _ [] = []
solutionsFromOpCombs nums (curOps:otherOps)
  | solution == 10 = mergedEq:(solutionsFromOpCombs nums otherOps)
  | otherwise = solutionsFromOpCombs nums otherOps
  where
    solution = read (solve mergedEq)::Double
    mergedEq = merge (map intToDigit nums) curOps

--  solutionsFromOpCombs :: [Int] -> [[Char]] -> [[Char]]
--solutionsFromOpCombs _ [] = []
--solutionsFromOpCombs nums (curOps:otherOps)
--  | isJust solution && fromJust solution == 10 = (merge (map (intToDigit) nums) curOps):(solutionsFromOpCombs nums otherOps)
--  | otherwise = solutionsFromOpCombs nums otherOps
--  where solution = solve nums curOps

---- get solution for a given list of digits and operators
--solve :: [Int] -> [Char] -> Maybe Int
--solve [digit1, digit2] [op] = subsolve digit1 digit2 op
--solve (digit1:digit2:otherDigits) (op:otherOps) = solution
--  where
--    solution
--      | isJust subsolution = solve ((fromJust subsolution):otherDigits) otherOps
--      | otherwise = Nothing
--    subsolution = subsolve digit1 digit2 op;

--subsolve :: Int -> Int -> Char -> Maybe Int
--subsolve num1 num2 '+' = Just (num1 + num2)
--subsolve num1 num2 '-' = Just (num1 - num2)
--subsolve num1 num2 '*' = Just (num1 * num2)
----subsolve num1 num2 '/' = num1 `quot` num2 -- need to return false if it is a fraction rather than rounding down!
--subsolve num1 num2 '/' = if (num1 `mod` num2) /= 0 then Nothing else Just (num1 `div` num2)

solve :: String -> String
solve eqString =
  do
    result <- runInterpreter (setImports ["Prelude"] >> eval eqString)
    case result of Left _ -> "0"; Right output -> output

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
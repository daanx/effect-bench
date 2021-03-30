{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    OverlappingInstances,
    UndecidableInstances,
    ConstraintKinds #-}


import Control.Monad
import Data.IORef
import Handlers
import TopLevel
import DesugarHandlers


[operation|Choose :: Int -> Int|]
[operation|forall a.None :: a|]

type CComp a = forall h.
  ([handles|h {Choose}|],[handles|h {None}|]) => Comp h a

[handler|
  ChooseAll a :: [a] 
    handles {Choose,None} where 
      Return  x   -> [x]
      Choose n k  -> concatMap k [1..n]     
      None k      -> []
|]


safe :: Int -> Int -> [Int] -> Bool
safe queen diag xs
  = case xs of
      [] -> True
      (q:qs) -> queen /= q &&
                queen /= q + diag &&
                queen /= q - diag &&
                safe queen (diag + 1) qs

findSolution :: Int -> Int -> CComp [Int]
findSolution n col 
  = if (col == 0) 
     then return [] 
     else do sol <- findSolution n (col - 1)
             queen <- choose n
             if (safe queen 1 sol)
               then seq sol $ return $! (queen : sol)     
               else none
                    
nqueens n = findSolution n n

main :: IO ()
main = putStrLn $ show $ length $ 
       chooseAll $
       nqueens 12
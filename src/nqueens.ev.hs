{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Ev.Eff
import Control.Ev.Util

safe :: Int -> Int -> [Int] -> Bool
safe queen diag xs
  = case xs of
      [] -> True
      (q:qs) -> queen /= q &&
                queen /= q + diag &&
                queen /= q - diag &&
                safe queen (diag + 1) qs

findSolution :: Int -> Int -> Eff (Choose :* e) [Int]
findSolution n col 
  = if (col == 0) 
     then return [] 
     else do sol <- findSolution n (col - 1)
             queen <- perform choose n
             if (safe queen 1 sol)
               then seq sol $ return $! (queen : sol)     
               else perform none ()
                    
nqueens n = findSolution n n

main :: IO ()
main = putStrLn $ show $ length $ 
       runEff $ 
       chooseAll $
       nqueens 12
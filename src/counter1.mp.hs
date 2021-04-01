{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Mp.Eff
import Control.Mp.Util

{-# INLINE xstate #-}  
xstate :: a -> Eff (State a :* e) ans -> Eff e ans
xstate init
  = handlerLocal init (State{ get = function (\_ -> localGet),
                              put = function (\x -> localPut x) })

counter :: State Int :? e => Int -> Eff e Int
counter n = do (i::Int) <- perform get ()
               if (i==0) 
                 then return n 
                 else do perform put (i - 1)
                         counter $! (n + 1)

main :: IO ()
main = putStrLn $ show $
       runEff $ 
       xstate (100100100::Int) $ 
       reader 0 $
       counter 0
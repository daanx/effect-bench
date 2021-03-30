{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Ev.Eff
import Control.Ev.Util 

-- note: adding INLINE has a huge effect as it allows GHC to inline the handler code completely away 
-- for this example. However, making the sample even a slighty bit more complex defeats the optimizer again.
{-# INLINE xstate #-}  
xstate :: a -> Eff (State a :* e) ans -> Eff e ans
xstate init
  = handlerLocal init (State{ get = function (\_ -> localGet),
                              put = function (\x -> localPut x) })

counter :: Int -> Eff (State Int :* e) Int
counter n = do (i::Int) <- perform get ()
               if (i==0) 
                 then return n 
                 else do perform put (i - 1)
                         counter $! (n + 1)

main :: IO ()
main = putStrLn $ show $ 
       runEff $ 
       xstate 100100100 $ 
       counter 0  
{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Ev.Eff
import Control.Ev.Util

{-# INLINE xstate #-}
xstate :: a -> Eff (State a :* e) ans -> Eff e ans
xstate init
  = handlerLocal init (State{ get = operation (\_ k -> do{ x <- localGet; k x }),
                              put = operation (\x k -> do{ localPut x; k () })    })



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
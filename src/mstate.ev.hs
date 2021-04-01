{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Ev.Eff
import Control.Ev.Util

-- | A state handler that takes an initial state of type @a@.
{-# INLINE mstate #-}
mstate :: a -> Eff (State a :* e) ans -> Eff e ans
mstate init action
  = do f  <- handlerRet (\x -> (\s -> return x)) 
                        (State{ get = operation (\_ k -> return (\s -> do f <- k s; f s)),
                                put = operation (\x k -> return (\s -> do f <- k (); f x)) })
                        action
       f init



counter :: Int -> Eff (State Int :* e) Int
counter n = do (i::Int) <- perform get ()
               if (i==0) 
                 then return n 
                 else do perform put (i - 1)
                         counter $! (n + 1)

main :: IO ()
main = putStrLn $ show $
       runEff $ 
       mstate 10100100 $ 
       counter 0
{-# LANGUAGE  ScopedTypeVariables, TypeOperators, FlexibleContexts, Rank2Types  #-}

module Main where
import Control.Ev.Eff
import Control.Ev.Util

data Yield e ans = Yield { yield :: !(Op (Int,Int,Int) () e ans) }

triples :: Int -> Int -> Eff (Choose :* Yield :* e) ()
triples n s
  = do x <- perform choose n
       y <- perform choose (x-1)
       z <- perform choose (y-1)
       if (x+y+z == s) 
         then perform yield (x,y,z)
         else perform none ()




choosing :: Eff (Choose :* e) () -> Eff e ()
choosing
  = handler  (Choose{ none   = except (\_ -> return ())
                    , choose = operation (\n k  -> let collect 0 = return ()
                                                       collect i = do k i
                                                                      collect (i-1)
                                                   in collect n)
                    })

count :: Eff (Yield :* e) () -> Eff e Int
count = handlerLocalRet (0::Int) (\_ cnt -> cnt) 
        (Yield{ 
          yield = function (\(x,y,z) -> 
                    do i <- localGet 
                       localPut (i+1)
                       return ()
                  ) 
        })

main :: IO ()
main = putStrLn $ show $
       runEff $ 
       count $
       choosing $
       triples 500 127
       
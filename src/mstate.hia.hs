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


[operation|Get s :: s|]
[operation|Put s :: s -> ()|]

type SComp s a = forall h.
  ([handles|h {Get s}|], [handles|h {Put s}|]) => Comp h a


[handler|
  NonParameterisedState s a :: (s -> a)
    handles {Get s, Put s} where
      Return  x     -> \_ -> x
      Get        k  -> \s -> k s s
      Put     s  k  -> \_ -> k () s
|]

counter :: Int -> SComp Int Int
counter n = do i <- get
               if (i==0) 
                 then return n
                 else do put (i-1) 
                         counter $! (n + 1)

main :: IO ()
main = putStrLn $ show $ 
       (nonParameterisedState (counter 0) 10100100)
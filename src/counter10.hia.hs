{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    OverlappingInstances,
    UndecidableInstances,
    ConstraintKinds 
#-}


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
  State s a :: s -> a 
    handles {Get s, Put s} where
      Return  x     _ -> x
      Get        k  s -> k s  s
      Put     s  k  _ -> k () s
|]

[handler|
  forward h.ForwardState s a :: s -> a 
    handles {Get s, Put s} where
      Return  x     _ -> return x
      Get        k  s -> k s  s
      Put     s  k  _ -> k () s
|]

[operation|Ask s :: s|]

[handler|
   Reader s a :: s -> a
     handles {Ask s} where
       Return x   _ -> x
       Ask      k s -> k s s
|]

[handler|
   forward h.ForwardReader s a :: s -> a
     handles {Ask s} where
       Return x   _ -> return x
       Ask      k s -> k s s
|]

type SRComp s a = forall h.
  ([handles|h {Get s}|], [handles|h {Put s}|], [handles|h {Ask s}|]) => Comp h a


counter :: Int -> SComp Int Int
counter n = do i <- get
               if (i==0) 
                 then return n
                 else do put (i-1)
                         counter $! (n + 1)

main :: IO ()
main = putStrLn $ (show :: Int -> String) $ 
       state (100100100::Int) $ 
       forwardReader 0 $
       forwardReader 1 $
       forwardReader 2 $
       forwardReader 3 $
       forwardReader 4 $
       forwardReader 5 $
       forwardReader 6 $
       forwardReader 7 $
       forwardReader 8 $
       forwardReader 9 $
       counter 0

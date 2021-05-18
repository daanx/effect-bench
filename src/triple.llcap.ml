open Printf

let handledNTriple_generated = (fun x0 -> (fun y1 -> (let x2 = (((((fun x3 -> (fun y4 -> (fun k5 -> (fun k6 -> (((let rec f7 x8 = (fun k9 -> (if (x8 <= 0) then (k9 ()) else (((let rec f10 x11 = (fun k12 -> (if (x11 <= 0) then (k12 ()) else (((let rec f13 x14 = (fun k15 -> (if (x14 <= 0) then (k15 ()) else (if (((x8 + x11) + x14) == y4) then (fun x16 -> (let x17 = ((k5 ()) (fun a18 -> ((f13 (x14 - 1)) k15))) in (x17 (x16 + 1)))) else ((f13 (x14 - 1)) k15)))) in f13) (x11 - 1)) (fun a13 -> ((f10 (x11 - 1)) k12))))) in f10) (x8 - 1)) (fun a10 -> ((f7 (x8 - 1)) k9))))) in f7) x3) k6))))) x0) y1) (fun a3 -> (fun k4 -> (k4 a3)))) (fun a3 -> (fun x4 -> x4))) in (x2 0))))

let _ = printf "%d\n" (handledNTriple_generated 500 127)


(*
Idris source:

module Examples.NTriple

import LambdaCapStaged

%access public export

Pick : Effect
Pick = Operation Int Int

Fail : Effect
Fail = Operation Unit Void

Yield : Effect
Yield = Operation (Int, Int, Int) Unit

absurd : Exp Void -> EFF rs a
absurd e = pure Void

voidEff : EFF rs Void -> EFF rs a
voidEff m = m >>= absurd


triple : Fun32 Yield Fail Pick Int Int rs Unit
triple = lam32 (\yield => \fail => \pick => \n => \s => do
  i <- call pick n
  j <- call pick (i - 1)
  k <- call pick (j - 1)
  if (i + j + k == s) {
    -- pure (Triple i j k)
    call yield (Triple i j k)
  } else {
    voidEff (call fail Unit)
  })




handledNTriple : Fun02 Int Int [] Int
handledNTriple = lam2 (\n => \s => do
  f <- withTry1 
     Yield (\t => \k => pure (lam1 (\s => call k Unit >>= \f => app1 f (s+1))))
     (\yield => do
         do withTry2
              Fail (\u => \k => do pure Unit)
              Pick (\n => \k => do
                let loop = letrec01 (\loop => \i => do
                            if (i <= 0) { do
                              pure Unit
                            } else { do
                              call k i
                              app1 loop (i-1)
                            })
                app1 loop n)
              (\fail => \pick => 
                do app32 triple (liftCAP yield) fail pick n s)
            pure (lam1 (\s => pure {a=Int} {rs=[]} s))  -- return final state
     )
  app1 f 0  
) 

-- putStrLn ("let handledNTriple_generated = " ++ prettyML 0 handledNTriple)
*)
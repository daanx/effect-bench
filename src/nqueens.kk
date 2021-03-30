module nqueens

import std/num/int32

// -----------------------------------------------------------------
// Shared
// -----------------------------------------------------------------

alias solution = list<int32>

fun is-safe( queen : int32, xs : solution, diag : int32 = one ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && queen.is-safe(qs,diag.inc))
    _ -> True
  }
}

// -----------------------------------------------------------------
// Hand-tuned, tail-recursive solution
// -----------------------------------------------------------------

alias solutions = list<list<int32>>

fun append-safe( queen : int32, xs : solution, xss : solutions ) : div solutions {
  if (queen <= zero) xss
  else {
   if (queen.is-safe(xs))
    then append-safe( queen.dec, xs, Cons(Cons(queen,xs),xss) )
    else append-safe( queen.dec, xs, xss )
  }
}

fun extend(queen : int32, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, append-safe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun find-solutions( n : int32, i : int32 ) : div solutions {
  if (i.is-zero) 
   then [[]]
   else extend(n, [], find-solutions(n, i.dec))  
}

public fun queens( n : int32 ) : div solutions {
  find-solutions(n,n)
}

// -----------------------------------------------------------------
// Elegant solution with user-defined effects
// -----------------------------------------------------------------

effect choose {
  control pick(n : int32) : int32
  control fail<b>()     : b
}

fun choose-all( action ) {
  handle(action){
    return(x){ [x] }
    control pick(n) { 
      list(1,n.int,fn(i){ resume(i.int32) } ).concat       
    }
    except fail(){ [] }
  }
}

fun find-solution( n : int32, col : int32 ) : choose solution {
  if (col.is-zero) then return []
  val sol    = find-solution(n, unsafe-decreasing(col.dec))
  val queen  = pick(n)
  if (queen.is-safe(sol)) then Cons(queen,sol) else fail()
}

fun queens-choose(n : int32 ) : solutions {
  choose-all{ find-solution(n,n) }
}


// -----------------------------------------------------------------
// Testing
// -----------------------------------------------------------------


fun main() {
  //queens(12.int32).length.println
  queens-choose(12.int32).length.println
}



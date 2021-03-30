import std/num/int32

effect st {
  control get() : int32
  control set(i : int32)  : ()
}

fun state( i : int32, action : () -> <st|e> a ) : e a {
  var s := i
  with {
    rcontrol get(){ rcontext.resume(s) }
    rcontrol set(x){ s := x; rcontext.resume(()) }    
  } 
  action()
}

fun counter( c : int32 ) : <st,div> int32 {
  val i = get()
  if (i==zero) then c else {
    set(i.dec)
    counter(c.inc)
  } 
}


// at this point (koka v2.0.16) koka does not implement a tail-call for
// a control resume; which means we use run out of stack on a 100M iteration...
// instead we run the test 100000 times over 1000 iterations. (That means we
// may get a bit faster if proper tail calling would be implemented)
fun test() {
  with state(1000.int32)
  counter(zero)
}

fun testN( n : int32, acc : int32 = zero) {
  if (n.is-zero) then println(acc.int) else {
    testN(n.dec, test() + acc)
  }
}

fun main() {
  testN(100100.int32)
  //test()
}
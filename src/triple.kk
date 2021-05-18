import std/num/int32

linear effect yield {
  fun yield(x:int32,y:int32,z:int32) : ()
}

fun count( action : () -> <yield|e> a ) : e int {
  var cnt := 0
  with return(x){ cnt }
  with fun yield(x,y,z){ cnt := cnt + 1 }
  action()
}


effect choose {
  control pick(n : int32) : int32
  control fail<b>() : b
}

fun choose( action ) {
  handle(action){
    rcontrol pick(n) { 
      fun loop(i:int32){ if (i.is-zero) then () else {
                          rcontext.resume(i)
                          loop(i.dec) 
                        }} 
      loop(n)
    }
    except fail(){ () }
  }
}

fun triple( n : int32, s : int32 ) : <choose,yield> () {
  val x = pick(n)
  val y = pick(x.dec)
  val z = pick(y.dec)
  if (x + y +z == s) then yield(x,y,z) else fail()
}

fun main() {
  with return(i:int){ println(i) }
  with count
  with choose
  triple(500.int32,127.int32)
}
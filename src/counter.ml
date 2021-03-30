open Printf

effect Put : int -> unit
let put v = perform (Put v)

effect Get : int
let get () = perform Get

let state init f =
  let s = ref init in
  match f () with
    | x -> x
    | effect (Put x) k -> (s := x; continue k ())
    | effect Get k -> let i : int = !s in continue k i 

let rec counter (c : int) =
  let i = get ()
  in if (i==0) 
      then c 
      else (put (i - 1); counter (c + 1))

let test () = let i = counter 0 in printf "%d\n" i 

let _ = state 100100100 test


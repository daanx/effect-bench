open Printf

effect Put : int -> unit
let put v = perform (Put v)

effect Get : int
let get () = perform Get

effect Ask : int
let ask () = perform Ask

let reader init action () = 
  try (printf "enter reader %d\n"  init;
       flush stdout;
       let x = action ()
       in (printf "leave reader %d\n" init; x))
  with 
    | effect Ask k -> continue k init

let state init f =
  let s = ref init in
  try 
    f ()
  with
    | effect (Put x) k -> (s := x; continue k ())
    | effect Get k -> let i : int = !s in continue k i 

let rec counter (c : int) =
  let i = get ()
  in if (i==0) 
      then c 
      else (put (i - 1); counter (c + 1))

let test () = let i = counter 0 in printf "%d\n" i 

let _ = state 100100100 
          (reader 0 (reader 1 (reader 2 (reader 3 (reader 4 (reader 5 (reader 6 (reader 7 (reader 8 (reader 9 (test))))))))))) 
          

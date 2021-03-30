open Printf 

effect Put : int -> unit
let put v = perform (Put v)

effect Get : int
let get () = perform Get

let state init f =
  let comp =
    match f () with
    | x -> (fun s -> x)
    | effect (Put s') k -> (fun _s -> continue k () s')
    | effect Get k -> (fun s -> continue k s s)
  in comp init

let rec counter (c : int) =
  let i = get ()
  in if (i==0) 
      then c 
      else (put (i - 1); counter (c + 1))

let test () = let i = counter 0 in printf "%d\n" i 

let _ = state 10100100 test



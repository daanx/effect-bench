
effect Yield : int*int*int -> unit
effect Pick : int -> int
effect Fail : 'a

let rec triple (n : int) (s : int) =
  let i = perform (Pick n) in
  let j = perform (Pick (i-1)) in
  let k = perform (Pick (j-1)) 
  in if (i + j + k == s) 
      then perform (Yield (i,j,k))
      else perform (Fail)
  

let yield_triples n s =
  try (triple n s)
  with
  | effect (Pick n) k ->
      let rec loop = fun i ->
        if (i<=0) then ()
        else if (i==1) then continue k i
                       else (continue (Obj.clone_continuation k) i; loop (i-1))
      in loop n
  | effect (Fail) k -> ();;

let count_triples n s =
  let cnt = ref 0 in
  try (yield_triples n s; !cnt)
  with
  | effect (Yield (x,y,z)) k -> (cnt := !cnt + 1; continue k ());;

Printf.printf "%8d\n" (count_triples 500 127);;

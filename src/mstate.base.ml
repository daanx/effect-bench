open Printf


let rec counter (n : int) =
  (fun s -> if (s==0) then n 
                      else (counter (n + 1)) (s - 1))

let test (init : int) = counter 0 init

let _ = printf "%d\n" (test 10100100)

 
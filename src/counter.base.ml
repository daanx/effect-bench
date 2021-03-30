open Printf

let test (init : int) =
  let cnt = ref init in
  let rec counter (n : int) = 
     let i = !cnt in
     if (i == 0) then n 
                 else (cnt := i - 1; counter (n + 1))
  in counter 0

let _ = printf "%d\n" (test 100100100)


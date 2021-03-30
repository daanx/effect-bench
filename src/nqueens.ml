open List;;

let rec safe queen diag xs = 
  match xs with
  | q :: qs -> queen <> q && queen <> q + diag && queen <> q - diag && safe queen (diag + 1) qs
  | [] -> true;;

(* direct solution *)
let rec append_safe queen xs xss =
  if (queen <= 0) then xss
  else if (safe queen 1 xs) then append_safe (queen - 1) xs ((queen :: xs) :: xss)
  else append_safe (queen - 1) xs xss;;

let rec extend queen acc xss =
  match xss with
  | xs :: rest -> extend queen (append_safe queen xs acc) rest
  | [] -> acc;;

let rec find_solutions n queen =
  if (queen == 0) then [[]]
   else extend n [] (find_solutions n (queen - 1));;
   
let nqueens n = List.length (find_solutions n n);;

(* effectful solution *)
effect Pick : int -> int
effect Fail : 'a

let rec find_solution (n : int) (col : int) =
  if (col == 0) 
    then [] 
    else let sol = find_solution n (col - 1) in
         let queen = perform (Pick n)
         in if (safe queen 1 sol) then queen :: sol else perform Fail;;

let chooseall action =
  try [action ()]
  with
  | effect (Pick n) k ->
      let rec loop = fun i ->
        if (i>n)  then []
        else if (i>=n) then [continue k i] 
                       else let xs = continue (Obj.clone_continuation k) i 
                            in xs :: loop (i + 1)
      in concat (loop 1)
  | effect (Fail) k -> [];;


let nqueens_choose (n : int) = List.length (chooseall (fun() -> find_solution n n));;

(*Printf.printf "%8d\n" (nqueens 12);;*)
Printf.printf "%8d\n" (nqueens_choose 12);;

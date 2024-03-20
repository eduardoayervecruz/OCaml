(* Problem 2 *)
let rec iter n f x = 
  match n with
  | 0 -> x
  | _ -> iter (n - 1) f (f x)

(* Test Functions 
let linear a = 3 * a + 2

let () = 
  Printf.printf "The result is: %d\n" (iter 5 linear 2)
;; *)
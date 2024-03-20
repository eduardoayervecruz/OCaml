(* Problem 1 *)
let rec sigma (a,b,f) =
  if a > b then 0 else f a + sigma(a+1, b, f)

(*let square a = a * a;;
let square_desc = "n^2";;

let print_sigma a b f f_desc =
  Printf.printf "Sum of elements with indexes from %d to %d with transformation f: %s is %d\n" a b f_desc (sigma(a, b, f))
;;

let () =
  print_sigma 1 5 square square_desc
;;*)

let rec last = function
| [] -> None
| [x] -> Some x
| _ :: t -> last t;;

let result_1 = last ["1";"2";"3";"4";"5"];;

let print_option opt = 
  match opt with
  | None -> print_endline "No value"
  | Some x -> print_endline ("Value: " ^ x);;

print_option result_1;;
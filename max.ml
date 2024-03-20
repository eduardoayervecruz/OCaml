let rec max_element lst = match lst with
  | [] -> None
  | [x] -> Some x
  | [x;y] -> if (x > y) then Some x else Some y
  | x :: y :: t -> if (x > y) then max_element (x :: t) else max_element (y :: t)
;;

let () = 
  let print_max lst = 
    let lst_string = String.concat "; " (List.map string_of_int lst) in 
    match max_element lst with 
    | None -> Printf.printf "Null list.\n"
    | Some max -> Printf.printf "Maximum element in list [%s] is: %d\n" lst_string max
  in
  print_max [];
  print_max [1];
  print_max [2;1];
  print_max [1;3;2];
  print_max [10;-1;0;10;25];
;;


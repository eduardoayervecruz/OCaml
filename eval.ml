type operator = Plus | Minus | Times | Divide;;
type expr = 
| Value of int
| Operation of expr * operator * expr;;

exception Division_by_zero;;

let rec eval (expression : expr) : int option = match expression with
  | Value n -> Some n
  | Operation (left, op, right) -> 
    (match(eval left, eval right) with
      | (Some l, Some r) -> (
          match op with
          | Plus -> Some (l + r)
          | Minus -> Some (l - r)
          | Times -> Some (l * r)
          | Divide -> if r != 0 then Some (l/r) else None
        )
      | _ -> None
    )
;;

let example_expr : expr = 
  Operation(
    Operation(Value(1), Plus, Value(2)),
    Times,
    Operation(Value(3), Minus, Value(4))
  );;

let result = eval example_expr;;

let print_result result = 
  match result with
    | Some value -> Printf.printf "The result is: %d\n" value
    | None -> print_endline "No value (maybe division by zero?)";;

print_result result;;

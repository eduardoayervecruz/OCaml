type ae = CONST of int
        | VAR of string
        | POWER of string * int
        | TIMES of ae list
        | SUM of ae list

let rec flatten expr =
  match expr with
  | TIMES lst ->
    let flattened_lst = List.concat (List.map flatten_helper lst) in
    TIMES flattened_lst
  | SUM lst ->
    let flattened_lst = List.concat (List.map flatten_helper lst) in
    SUM flattened_lst
  | _ -> expr

and flatten_helper expr =
  match expr with
  | TIMES lst -> 
    begin match List.concat (List.map flatten_helper lst) with
    | [] -> []  (* Remove empty TIMES *)
    | lst_inner -> [TIMES lst_inner]
    end
  | SUM lst -> 
    begin match List.concat (List.map flatten_helper lst) with
    | [] -> []  (* Remove empty SUM *)
    | lst_inner -> [SUM lst_inner]
    end
  | _ -> [flatten expr]

let rec simplify expr =
  let expr = flatten expr in 
  match expr with
    | TIMES lst ->
      let flattened_lst = if List.exists (function TIMES _ -> true | _ -> false) lst then
        List.concat (List.map (function
                                | TIMES l -> l
                                | x -> [x]) lst)
      else lst in
      let constants, terms = List.partition (function CONST _ -> true | _ -> false) flattened_lst in
      let product_of_constants = List.fold_left (fun acc e -> match e with CONST n -> acc * n | _ -> acc) 1 constants in
      let simplified_terms = List.filter (fun e -> e <> CONST 1) (List.map simplify terms) in
      let result_terms =
      if product_of_constants = 0 then [CONST 0]
      else if product_of_constants = 1 then simplified_terms
      else CONST product_of_constants :: simplified_terms in
      begin match result_terms with
      | [] -> CONST 1
      | [hd] -> hd
      | _ -> TIMES result_terms
      end
    | SUM lst ->
        let simplified_lst = List.map simplify lst in
        let filtered_lst = List.filter (function CONST 0 -> false | _ -> true) simplified_lst in
        begin match filtered_lst with
        | [] -> CONST 0
        | [hd] -> hd
        | _ -> SUM filtered_lst
        end
    | POWER (x, n) as power ->
        if n = 0 then CONST 1
        else if n = 1 then VAR x
        else power
    | CONST _ | VAR _ as leaf -> leaf

let rec diff (expr, var) =
  match expr with
  | CONST _ -> CONST 0
  | VAR x -> if x = var then CONST 1 else CONST 0
  | POWER (x, n) ->
      if x = var then 
        if n = 1 then CONST 1
        else simplify (TIMES [CONST n; POWER (x, n - 1)])
      else CONST 0
  | TIMES lst ->
      if List.exists (function CONST 0 -> true | _ -> false) lst then CONST 0 
      else
        (match lst with
          | [] -> CONST 1
          | [hd] -> simplify (diff (hd, var))
          | hd::tl -> simplify (SUM [TIMES (diff (hd, var) :: tl); TIMES [hd; diff (TIMES tl, var)]])
        )
  | SUM lst -> simplify (SUM (List.map (fun e -> diff (e, var)) lst))

let rec string_of_ae = function
  | CONST n -> string_of_int n
  | VAR x -> x
  | POWER (x, n) -> x ^ "^" ^ string_of_int n
  | TIMES lst -> String.concat " * " (List.map string_of_ae lst)
  | SUM lst -> String.concat " + " (List.map string_of_ae lst)

let rec raw_ae expr =
  match expr with
  | CONST n -> "CONST " ^ string_of_int n
  | VAR x -> "VAR \"" ^ x ^ "\""
  | POWER (x, n) -> "POWER (\"" ^ x ^ "\", " ^ string_of_int n ^ ")"
  | TIMES lst -> 
      "TIMES [" ^ String.concat "; " (List.map raw_ae lst) ^ "]"
  | SUM lst -> 
      "SUM [" ^ String.concat "; " (List.map raw_ae lst) ^ "]"

(* Test cases *)
let () =
  let expr1 = SUM [TIMES [CONST 2; POWER ("x", 2)]; TIMES [VAR "x"; CONST 3]; CONST 5] in
  let expr2 = SUM [POWER ("x", 3); CONST 5] in
  let expr3 = SUM [TIMES [POWER("x", 2); POWER ("x", 4)]; TIMES [VAR "y"; CONST 4]; TIMES[CONST 4; POWER ("x", 5)]] in

  let diff_expr1 = diff (expr1, "x") in
  let diff_expr2 = diff (expr2, "x") in
  let diff_expr3 = diff (expr3, "x") in

  Printf.printf "The derivative of expr1 with respect to x is: %s\n" (raw_ae diff_expr1);
  Printf.printf "The derivative of expr2 with respect to x is: %s\n" (raw_ae diff_expr2);
  Printf.printf "The derivative of expr2 with respect to x is: %s\n" (raw_ae diff_expr3);

  Printf.printf "The derivative of expr1 with respect to x is: %s\n" (string_of_ae diff_expr1);
  Printf.printf "The derivative of expr2 with respect to x is: %s\n" (string_of_ae diff_expr2);
  Printf.printf "The derivative of expr2 with respect to x is: %s\n" (string_of_ae diff_expr3);
;;
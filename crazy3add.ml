(* Problem 5 *)
type crazy3 = NIL 
            | ZERO of crazy3 
            | ONE of crazy3
            | MONE of crazy3
            | TWO of crazy3
            | MTWO of crazy3

let rec crazy3val c3 =
  let rec aux c3 pow =
    match c3 with
    | NIL -> 0
    | ZERO rest -> aux rest (pow * 3)
    | ONE rest -> (pow) + aux rest (pow * 3)
    | MONE rest -> (-pow) + aux rest (pow * 3)
    | TWO rest -> (pow * 2) + aux rest (pow * 3)
    | MTWO rest -> (pow * (-2)) + aux rest (pow * 3)
  in 
  aux c3 1

let val3crazy n = 
  let rec aux2 n acc =
    if n = 0 then acc
    else
      let digit = 
        match n mod 3 with 
        | 0 -> ZERO acc
        | 1 -> ONE acc
        | -1 -> MONE acc
        | 2 -> TWO acc
        | -2 -> MTWO acc
        | _ -> failwith "unexpected remainder"
      in
      aux2 (n/3) digit 
  in
  aux2 n NIL

let rec crazy3add c1 c2 = val3crazy (crazy3val (c1) + crazy3val (c2)) 

(* Testcases *)
(*  
let rec string_of_crazy3 = function
  | NIL -> "NIL"
  | ZERO rest -> "ZERO(" ^ string_of_crazy3 rest ^ ")"
  | ONE rest -> "ONE(" ^ string_of_crazy3 rest ^ ")"
  | MONE rest -> "MONE(" ^ string_of_crazy3 rest ^ ")"
  | TWO rest -> "TWO(" ^ string_of_crazy3 rest ^ ")"
  | MTWO rest -> "MTWO(" ^ string_of_crazy3 rest ^ ")"

let () = 
  let tests = [
    NIL;
    MONE(MTWO(ZERO(NIL)));
    ZERO(NIL); 
    MONE(TWO(ZERO(NIL)));
    MONE(ONE(ZERO(NIL)));
  ] in
  let addTests = [
    (MONE(MTWO(ZERO(NIL))), MONE(ZERO(NIL)));
    (ZERO(NIL), MONE(TWO(ZERO(NIL))));
    (MONE(ONE(ZERO(NIL))), MONE(ONE(ZERO(NIL))))
  ] in 
  List.iter (fun testcase ->
    Printf.printf "Result: %d\n" (crazy3val testcase)
  ) tests;
  List.iter (fun (c1, c2) -> 
    Printf.printf "Sum Result: %s\n" (string_of_crazy3 (crazy3add c1 c2))
  ) addTests
;; *)


(* Problem 4 *)
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
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let string_of_team = function
  | Korea -> "Korea" | France -> "France" | Usa -> "Usa" | Brazil -> "Brazil"
  | Japan -> "Japan" | Nigeria -> "Nigeria" | Cameroon -> "Cameroon"
  | Poland -> "Poland" | Portugal -> "Portugal" | Italy -> "Italy"
  | Germany -> "Germany" | Norway -> "Norway" | Sweden -> "Sweden"
  | England -> "England" | Argentina -> "Argentina"

let rec parenize = function
  | LEAF team -> string_of_team team
  | NODE (t1, t2) -> "(" ^ parenize t1 ^ " " ^ parenize t2 ^ ")"

let () =
  let tests = [
    NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil);
    NODE(NODE(LEAF France, LEAF Argentina), NODE(LEAF England, LEAF Poland));
    NODE(LEAF Italy, NODE(LEAF Sweden, LEAF Germany))
  ] in
  List.iter (fun testexpr ->
    Printf.printf "Result: %s\n" (parenize testexpr)
  ) tests
;;


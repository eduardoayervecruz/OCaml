type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec check metro valid_names = match metro with
  | STATION name -> List.mem name valid_names
  | AREA (name, metro) -> check metro (name :: valid_names)
  | CONNECT (metro1, metro2) -> check metro1 valid_names && check metro2 valid_names

let checkMetro metro = check metro []

let print_bool b = print_endline (string_of_bool b)

let () =
  print_bool (checkMetro (AREA("a", STATION "a")));
  print_bool (checkMetro (AREA("a", AREA("a", STATION "a"))));
  print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
  print_bool (checkMetro (CONNECT(AREA("a", STATION "a"), AREA("b", STATION "b"))));
  print_bool (checkMetro (AREA("c", CONNECT(AREA("a", STATION "a"), AREA("b", STATION "b")))));
  
  print_bool (checkMetro (AREA("a", STATION "b")));
  print_bool (checkMetro (CONNECT(STATION "a", STATION "b")));
  print_bool (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
  print_bool (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
  print_bool (checkMetro (CONNECT(AREA("a", STATION "a"), STATION "b")))
;;
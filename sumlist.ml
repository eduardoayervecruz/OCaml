let rec merge (lst1, lst2) = 
  match (lst1, lst2) with 
    | ([], lst) | (lst, []) -> lst
    | (h1 :: t1, h2 :: t2) ->
      if(h1 > h2) then h1 :: merge (t1, lst2)
      else if (h1 < h2) then h2 :: merge (lst1, t2)
      else h1 :: merge (t1, t2)
;;

let () = 
  let lst1 = [5;3;2] in 
  let lst2 = [6;4;2] in
  let merged_list = merge (lst1, lst2) in 
  List.iter (Printf.printf " %d") merged_list;
  Printf.printf "\n"
;;
  
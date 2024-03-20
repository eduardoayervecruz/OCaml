type heap = EMPTY 
          | NODE of rank * value * heap * heap 
and rank = int
and value = int

exception EmptyHeap
let rank h =
  match h with
  | EMPTY -> -1
  | NODE(r, _, _, _) -> r
  
let findMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x

let shake (x, lh, rh) =
  if rank lh >= rank rh
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let rec merge (h1, h2) =
  match (h1, h2) with
  | EMPTY, _ -> h2
  | _, EMPTY -> h1
  | NODE(r1, v1, lh1, rh1), NODE(r2, v2, lh2, rh2) ->
      if v1 <= v2
      then let merged = merge (rh1, h2) in
            shake (v1, lh1, merged)
      else let merged = merge (h1, rh2) in
            shake (v2, lh2, merged)

let insert (x, h) = merge (h, NODE(0, x, EMPTY, EMPTY))

let deleteMin h =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, _, lh, rh) -> merge (lh, rh)

let () =
  let heap1 = NODE(1, 10, EMPTY, EMPTY) in
  let heap2 = NODE(1, 20, EMPTY, EMPTY) in
  let heap3 = NODE(1, 30, EMPTY, EMPTY) in
  let heap4 = NODE(1, 40, EMPTY, EMPTY) in
  let heap5 = NODE(2, 15, heap1, heap2) in
  let heap6 = NODE(2, 25, heap3, heap4) in

  let merged1 = merge (heap1, heap2) in
  let merged2 = merge (heap2, heap3) in
  let merged3 = merge (heap5, heap3) in
  let merged4 = merge (heap6, heap1) in
  let merged5 = merge (heap5, heap6) in

  let print_heap h =
    let rec to_string = function
      | EMPTY -> "."
      | NODE(_, v, lh, rh) -> Printf.sprintf "(%d, %s, %s)" v (to_string lh) (to_string rh)
    in
    print_endline (to_string h)
  in

  print_endline "Merged heaps:";
  print_heap merged1;
  print_heap merged2;
  print_heap merged3;
  print_heap merged4;
  print_heap merged5
;;
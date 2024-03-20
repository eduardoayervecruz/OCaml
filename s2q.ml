module type Queue = sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

module IntListQ = struct
  type element = int list
  type queue = element list * element list
  exception EMPTY_Q

  let emptyQ = ([], [])

  let enQ (q, elem) =
    let (ins_stack, del_stack) = q in
    (elem :: ins_stack, del_stack)

  let deQ q =
    let (ins_stack, del_stack) = q in
    match del_stack with
    | hd :: tl -> (hd, (ins_stack, tl))
    | [] ->
      match List.rev ins_stack with
      | [] -> raise EMPTY_Q
      | hd :: tl -> (hd, ([], tl))
end

(* Testing the IntListQ module *)
let () =
  let open IntListQ in

  (* Test creating an empty queue *)
  let q = emptyQ in
  Printf.printf "Created an empty queue.\n";

  (* Test enqueuing elements *)
  let q = enQ (q, [1]) in
  let q = enQ (q, [2; 3]) in
  let q = enQ (q, [4; 5; 6]) in
  Printf.printf "Enqueued elements: [1], [2; 3], [4; 5; 6]\n";

  (* Test dequeuing elements *)
  let (item1, q) = deQ q in
  Printf.printf "Dequeued: %s\n" (String.concat "; " (List.map string_of_int item1));

  let (item2, q) = deQ q in
  Printf.printf "Dequeued: %s\n" (String.concat "; " (List.map string_of_int item2));

  let (item3, q) = deQ q in
  Printf.printf "Dequeued: %s\n" (String.concat "; " (List.map string_of_int item3));

  (* Test dequeuing until empty *)
  let q = enQ (emptyQ, [7]) in
  let (_, q) = deQ q in
  try
    let _ = deQ q in
    Printf.printf "Error: Queue should be empty but deQ did not raise EMPTY_Q\n";
  with
  | EMPTY_Q -> Printf.printf "Passed: EMPTY_Q raised as expected when queue is empty\n";
  | _ -> Printf.printf "Error: An unexpected exception was raised\n";

  Printf.printf "Testing complete.\n";
;;
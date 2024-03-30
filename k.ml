module type KMINUS = sig
  exception Error of string

  type id = string
  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp
    | IF of exp * exp * exp
    | WHILE of exp * exp
    | LETV of id * exp * exp
    | ASSIGN of id * exp
    | READ of id
    | WRITE of exp
  type program = exp
  type memory
  type env
  type value = 
    | VInt of int
    | VBool of bool
    | VUnit
    | VError of string

  val emptyMemory: unit -> memory
  val emptyEnv: unit -> env
  val run: memory * env * program -> value
end

module KInterpreter : KMINUS = struct
  open Printf
  exception Error of string

  type id = string
  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp
    | IF of exp * exp * exp
    | WHILE of exp * exp
    | LETV of id * exp * exp
    | ASSIGN of id * exp
    | READ of id
    | WRITE of exp
  type program = exp
  type value = 
    | VInt of int
    | VBool of bool
    | VUnit
    | VError of string
  
  type memory = (id, value) Hashtbl.t
  type env = memory

  let emptyMemory () : memory = Hashtbl.create 100
  let emptyEnv () : env = emptyMemory ()

  let eval_error msg = VError msg

  let rec eval (mem: memory) (e: exp) : value =
    match e with
    | NUM n -> VInt n
    | TRUE -> VBool true
    | FALSE -> VBool false
    | UNIT -> VUnit
    | VAR x -> (try Hashtbl.find mem x with Not_found -> eval_error ("Unbound variable: " ^ x))
    | ADD (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt n2) -> VInt (n1 + n2)
       | _ -> eval_error "ADD: Type error")
    | SUB (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt n2) -> VInt (n1 - n2)
       | _ -> eval_error "SUB: Type error")
    | MUL (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt n2) -> VInt (n1 * n2)
       | _ -> eval_error "MUL: Type error")
    | DIV (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt 0) -> eval_error "DIV: Division by zero"
       | (VInt n1, VInt n2) -> VInt (n1 / n2)
       | _ -> eval_error "DIV: Type error")
    | EQUAL (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt n2) -> VBool (n1 = n2)
       | _ -> eval_error "EQUAL: Type error")
    | LESS (e1, e2) ->
      (match (eval mem e1, eval mem e2) with
       | (VInt n1, VInt n2) -> VBool (n1 < n2)
       | _ -> eval_error "LESS: Type error")
    | NOT e ->
      (match eval mem e with
       | VBool b -> VBool (not b)
       | _ -> eval_error "NOT: Type error")
    | SEQ (e1, e2) ->
      let _ = eval mem e1 in
      eval mem e2
    | IF (e1, e2, e3) ->
      (match eval mem e1 with
       | VBool true -> eval mem e2
       | VBool false -> eval mem e3
       | _ -> eval_error "IF: Type error")
    | WHILE (cond, body) ->
      let rec loop () =
        match eval mem cond with
        | VBool true -> let _ = eval mem body in loop ()
        | VBool false -> VUnit
        | _ -> eval_error "WHILE: Type error"
      in loop ()
    | LETV (x, e1, e2) ->
      let v1 = eval mem e1 in
      Hashtbl.add mem x v1;
      let result = eval mem e2 in
      Hashtbl.remove mem x;
      result
    | ASSIGN (x, e) ->
      let v = eval mem e in
      if Hashtbl.mem mem x then
        (Hashtbl.replace mem x v; v)
      else
        eval_error ("Unbound variable in ASSIGN: " ^ x)
    | READ x ->
      let n = read_int () in
      Hashtbl.add mem x (VInt n);
      VUnit
    | WRITE e ->
      (match eval mem e with
       | VInt n -> print_endline (string_of_int n); VUnit
       | _ -> eval_error "WRITE: Type error")

    let run ((mem, env, p) : memory * env * program) : value =
      eval mem p
    end


(* Define the test_interpreter function *)
let test_interpreter program =
  let mem = KInterpreter.emptyMemory () in
  let env = KInterpreter.emptyEnv () in
  KInterpreter.run (mem, env, program)

let () =

  let string_of_value value =
    match value with
    | KInterpreter.VInt n -> Printf.sprintf "Int %d" n
    | KInterpreter.VBool b -> Printf.sprintf "Bool %b" b
    | KInterpreter.VUnit -> "Unit"
    | KInterpreter.VError msg -> Printf.sprintf "Error: %s" msg
  in

  let print_result exp result =
    Printf.printf "Result of %s is %s\n" exp (string_of_value result)
  in

  (* Test case 1: Simple arithmetic *)
  let program1 = KInterpreter.ADD (KInterpreter.NUM 10, KInterpreter.NUM 20) in
  let result1 = test_interpreter program1 in
  print_result "10 + 20" result1;

  (* Test case 2: Variable binding and evaluation *)
  let program2 = KInterpreter.LETV ("x", KInterpreter.NUM 10, KInterpreter.VAR "x") in
  let result2 = test_interpreter program2 in
  print_result "let x = 10 in x" result2;

  (* Test case 3: Conditional true branch *)
  let program3 = KInterpreter.IF (KInterpreter.TRUE, KInterpreter.NUM 10, KInterpreter.NUM 20) in
  let result3 = test_interpreter program3 in
  print_result "if true then 10 else 20" result3;

  (* Test case 4: Conditional false branch *)
  let program4 = KInterpreter.IF (KInterpreter.FALSE, KInterpreter.NUM 10, KInterpreter.NUM 20) in
  let result4 = test_interpreter program4 in
  print_result "if false then 10 else 20" result4;

  (* Test case 5: Sequence of expressions *)
  let program5 = KInterpreter.SEQ (KInterpreter.ADD (KInterpreter.NUM 1, KInterpreter.NUM 2),
                                  KInterpreter.SUB (KInterpreter.NUM 10, KInterpreter.NUM 5)) in
  let result5 = test_interpreter program5 in
  print_result "(1 + 2); (10 - 5)" result5;

  (* Test case 6: While loop (to be executed interactively due to possible infinite loop) *)
  let program6 = KInterpreter.WHILE (KInterpreter.FALSE, KInterpreter.WRITE (KInterpreter.NUM 1)) in
  let result6 = test_interpreter program6 in
  print_result "while false do write 1" result6;

  (* Test case 7: Error due to unbound variable *)
  let program7 = KInterpreter.VAR "y" in
  let result7 = test_interpreter program7 in
  print_result "y" result7;

  (* Test case 8: Division by zero error (interpreter must handle this properly) *)
  let program8 = KInterpreter.DIV (KInterpreter.NUM 10, KInterpreter.NUM 0) in
  let result8 = test_interpreter program8 in
  print_result "10 / 0" result8;
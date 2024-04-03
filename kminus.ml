(*
 * SNU 4190.310 Programming Languages 2024 Spring
 *  K- Interpreter
 *)

(** Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(** Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t
  (** get empty memory *)

  val load : 'a t -> Loc.t -> 'a
  (** load value : Mem.load mem loc => value *)

  val store : 'a t -> Loc.t -> 'a -> 'a t
  (** save value : Mem.store mem loc value => mem' *)

  val alloc : 'a t -> Loc.t * 'a t
  (** get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t
  (** get empty environment *)

  val lookup : ('a, 'b) t -> 'a -> 'b
  (** lookup environment : Env.lookup env key => content *)

  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** id binding : Env.bind env key content => env'*)
end

(** Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(** Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(**  K- Interpreter *)
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
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
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
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)
  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with Unit -> () | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with Record r -> r | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env)
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    match e with
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x -> (Mem.load mem (lookup_env_loc env x), mem)
    | ADD (e1, e2) -> binary_op mem env e1 e2 ( + )
    | SUB (e1, e2) -> binary_op mem env e1 e2 ( - )
    | MUL (e1, e2) -> binary_op mem env e1 e2 ( * )
    | DIV (e1, e2) -> binary_op mem env e1 e2 ( / )
    | EQUAL (e1, e2) -> comparison_op mem env e1 e2 ( = )
    | LESS (e1, e2) -> comparison_op mem env e1 e2 ( < )
    | NOT e -> unary_op mem env e (not)
    | SEQ (e1, e2) -> let _, mem' = eval mem env e1 in eval mem' env e2
    | IF (e1, e2, e3) -> 
        let v, mem' = eval mem env e1 in
        (match v with
        | Bool true -> eval mem' env e2
        | Bool false -> eval mem' env e3
        | _ -> raise (Error "IF condition must be a boolean"))
    | WHILE (e1, e2) ->
        let rec loop (v, m) =
          match v with
          | Bool true -> let _, m' = eval m env e2 in loop (eval m' env e1)
          | Bool false -> (Unit, m)
          | _ -> raise (Error "WHILE condition must be a boolean")
        in loop (eval mem env e1)
    | LETV (x, e1, e2) ->
        let v, mem' = eval mem env e1 in
        let l, mem'' = Mem.alloc mem' in
        eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (f, ids, e1, e2) ->
      let env' = Env.bind env f (Proc (ids, e1, env)) in
      eval mem env' e2
    | CALLV (f, exps) ->
      let param_ids, body, func_env = lookup_env_proc env f in
      let arg_vals, mem' = List.fold_right
        (fun e (vals, m) ->
          let v, m' = eval m env e in
          (v :: vals, m'))
        exps ([], mem) in
      let new_env, mem'' = List.fold_left2
        (fun (e, m) param_id arg_val ->
          let loc, m' = Mem.alloc m in
          let e' = Env.bind e param_id (Addr loc) in
          let m'' = Mem.store m' loc arg_val in
          (e', m''))
        (func_env, mem') param_ids arg_vals in
      eval mem'' new_env body
    | CALLR (f, ids) ->
      let args, body, func_env = lookup_env_proc env f in
      let new_env = List.fold_left2 (fun e arg id ->
        let loc = lookup_env_loc env id in  
        Env.bind e arg (Addr loc)) func_env args ids in
      eval mem new_env body
    | RECORD fields ->
      let rec store_fields fs mem env =
      match fs with
      | [] -> (Record (fun id -> lookup_env_loc env id), mem)
      | (id, e)::rest ->
      let v, mem' = eval mem env e in
      let l, mem'' = Mem.alloc mem' in
      store_fields rest (Mem.store mem'' l v) (Env.bind env id (Addr l))
      in
      store_fields fields mem env
    | FIELD (e, id) ->
      let r, mem' = eval mem env e in
      begin match r with
      | Record f -> (Mem.load mem' (f id), mem')
      | _ -> raise (Error "TypeError : not record")
      end
    | ASSIGN (x, e) ->
      let v, mem' = eval mem env e in
      let loc = lookup_env_loc env x in
      (v, Mem.store mem' loc v)
    | ASSIGNF (e1, id, e2) ->
      let record_val, mem' = eval mem env e1 in
      begin match record_val with
      | Record record_func ->
          let value_to_assign, mem'' = eval mem' env e2 in
          (value_to_assign, Mem.store mem'' (record_func id) value_to_assign)
      | _ -> raise (Error "TypeError: not a record")
      end
    | READ x ->
        let v = Num (read_int ()) in
        let l = lookup_env_loc env x in
        (v, Mem.store mem l v)
    | WRITE e ->
        let v, mem' = eval mem env e in
        let n = value_int v in
        let _ = print_endline (string_of_int n) in
        (v, mem')
    and binary_op mem env e1 e2 op =
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Num (op (value_int v1) (value_int v2)), mem2)
    and comparison_op mem env e1 e2 op =
      let (v1, mem1) = eval mem env e1 in
      let (v2, mem2) = eval mem1 env e2 in
      (Bool (op (value_int v1) (value_int v2)), mem2)
    and unary_op mem env e op =
      let (v, mem') = eval mem env e in
      (Bool (op (value_bool v)), mem')
    
  let run (mem, env, pgm) =
    let v, _ = eval mem env pgm in
    v
end

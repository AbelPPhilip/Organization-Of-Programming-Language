open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(* Adds mapping [x:v] to environment [env] *)


let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
  | Value v -> v 
  | ID x -> ref_lookup env x 
  | Fun (x,e) -> Closure(env, x, e)
  | Not e -> 
    (match eval_expr env e with 
    | Bool b -> Bool(not b)
    | _ -> raise(TypeError "No boolean value for operator lmao"))
  | Binop(op, e1, e2) ->
    let v1 = eval_expr env e1 in 
    let v2 = eval_expr env e2 in 
    eval_binop op v1 v2
  | If (cond, e_then, e_else) -> 
    (match (eval_expr env cond) with
    |Bool b -> if b then eval_expr env e_then else eval_expr env e_else
    |_-> raise (TypeError "If condition should be boolean bruh"))
  | FunctionCall (f,arg) -> 
    (match (eval_expr env f) with 
    |Closure (env',x,body) ->
      let arg_value = eval_expr env arg in
      let new_env = ref_extend env' x arg_value in
      eval_expr new_env body
    |_ -> raise (TypeError "need a function in function call"))
  | Let (x, is_rec, e1, e2) -> 
    if is_rec then 
      let env' = ref_extend_tmp env x in 
      let v1 = eval_expr env' e1 in 
      let() = ref_update env' x v1 in 
      eval_expr env' e2
    else
      let v1 = eval_expr env e1 in 
      let env' = ref_extend env x v1 in 
      eval_expr env' e2 
  and eval_binop op v1 v2 =
    match op, v1, v2 with 
    | Add, Int v1, Int v2 -> Int(v1 + v2)
    | Sub, Int v1, Int v2 -> Int(v1 - v2)
    | Mult, Int v1, Int v2 -> Int(v1 * v2)
    | Div, Int v1, Int v2 -> if v2 = 0 then raise DivByZeroError else Int(v1/v2)
    | Concat, String v1, String v2 -> String (v1 ^ v2)
    | And, Bool v1, Bool v2 -> Bool(v1 && v2)
    | Or, Bool v1, Bool v2 -> Bool(v1 || v2)
    | Equal, Int v1, Int v2 -> Bool(v1 = v2)
    | NotEqual, Int v1, Int v2 -> Bool(v1 <> v2)
    | Less, Int v1, Int v2 -> Bool (v1 < v2)
    | Greater, Int v1, Int v2 -> Bool(v1 > v2)
    | LessEqual, Int v1, Int v2 -> Bool(v1 <= v2)
    | GreaterEqual, Int v1, Int v2 -> Bool(v1 >= v2)
    | _ -> raise (TypeError "operand types for binary operation")

(* Part 2: Evaluating mutop directive *)
let eval_mutop env m = 
  match m with 
  | Def (x,e) ->
    let v = eval_expr env e in 
    (ref_extend env x v, Some v)
  | Expr e ->
    let v = eval_expr env e in 
    (env, Some v)
  | NoOp -> (env, None)

open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(*
  Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr
  LetExpr -> let Recursion Tok_ID = Expr in Expr
  Recursion -> rec | ε
  FunctionExpr -> fun Tok_ID -> Expr
  IfExpr -> if Expr then Expr else Expr
  OrExpr -> AndExpr || OrExpr | AndExpr
  AndExpr -> EqualityExpr && AndExpr | EqualityExpr
  EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
  EqualityOperator -> = | <>
  RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
  RelationalOperator -> < | > | <= | >=
  AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
  AdditiveOperator -> + | -
  MultiplicativeExpr -> ConcatExpr MultiplicativeOperator MultiplicativeExpr | ConcatExpr
  MultiplicativeOperator -> * | /
  ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr
  UnaryExpr -> not UnaryExpr | FunctionCallExpr
  FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr
  PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)

let rec parse_expr toks = 
  match lookahead toks with 
  |Some Tok_Let -> 
    let (toks, expr) = parse_LetExpr toks in (toks, expr)
  |Some Tok_If -> 
    let (toks, expr) = parse_IfExpr toks in (toks, expr)
  |Some Tok_Fun -> 
    let (toks, expr) = parse_FunExpr toks in (toks, expr)
  |_ -> 
    let (toks, expr) = parse_OrExpr toks in (toks, expr)
  
  and parse_LetExpr toks = 
    let toks = match_token toks Tok_Let in 
    let toks, isRec  = parse_Rec toks in  
    (match lookahead toks with
    | Some(Tok_ID id) -> 
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let toks, rhs = parse_expr toks in
      let toks = match_token toks Tok_In in
      let toks, body = parse_expr toks in 
      (toks, Let(id, isRec, rhs, body))
    | _ -> raise (InvalidInputException "Expected identifier after let"))

  and parse_Rec toks = 
    match lookahead toks with 
    |Some Tok_Rec -> 
      let toks = match_token toks Tok_Rec in 
      (toks, true)
    | _ -> (toks,false)
  
  and parse_IfExpr toks = 
    let toks = match_token toks Tok_If in 
    let toks, condExpr = parse_expr toks in 
    let toks = match_token toks Tok_Then in 
    let toks, thenExpr = parse_expr toks in 
    let toks = match_token toks Tok_Else in 
    let toks, elseExpr = parse_expr toks in 
    (toks, If (condExpr, thenExpr, elseExpr))
  
  and parse_FunExpr toks =
    let toks = match_token toks Tok_Fun in 
    (match lookahead toks with 
    |Some (Tok_ID id) ->
      let toks = match_token toks(Tok_ID id) in
      let toks = match_token toks Tok_Arrow in 
      let toks, body = parse_expr toks in
      (toks, Fun (id, body))
    |_-> raise(InvalidInputException "Unexpected identifier after fun"))
  
  and parse_OrExpr toks = 
    let toks, lhs = parse_AndExpr toks in 
    match lookahead toks with 
    | Some Tok_Or ->
      let toks = match_token toks Tok_Or in 
      let toks, rhs = parse_OrExpr toks in 
      (toks, Binop(Or, lhs, rhs))
    | _-> (toks, lhs) 
  
  and parse_AndExpr toks = 
    let toks, lhs = parse_EqualityExpr toks in 
    match lookahead toks with 
    | Some Tok_And -> 
      let toks = match_token toks Tok_And in 
      let toks, rhs = parse_AndExpr toks in 
      (toks, Binop(And, lhs, rhs))
    |_ -> (toks, lhs)
  
  and parse_EqualityExpr toks = 
    let toks, lhs = parse_relationalExpr toks in 
    match lookahead toks with 
    | Some Tok_Equal -> 
      let toks = match_token toks Tok_Equal in 
      let toks, rhs = parse_EqualityExpr toks in 
      (toks, Binop(Equal, lhs, rhs))
    | Some Tok_NotEqual ->
      let toks = match_token toks Tok_NotEqual in 
      let toks, rhs = parse_EqualityExpr toks in 
      (toks, Binop(NotEqual, lhs, rhs))
    | _ -> (toks, lhs)
    
  and parse_relationalExpr toks = 
    let toks, lhs = parse_AdditiveExpr toks in 
    match lookahead toks with 
    | Some Tok_Greater -> 
      let toks = match_token toks Tok_Greater in 
      let toks, rhs = parse_relationalExpr toks in 
      (toks, Binop(Greater,lhs,rhs))
    | Some Tok_Less -> 
      let toks = match_token toks Tok_Less in 
      let toks, rhs = parse_relationalExpr toks in 
      (toks,Binop(Greater, lhs, rhs))
    | Some Tok_GreaterEqual -> 
      let toks = match_token toks Tok_GreaterEqual in 
      let toks, rhs  = parse_relationalExpr toks in 
      (toks, Binop(GreaterEqual, lhs, rhs))
    | Some Tok_LessEqual ->
      let toks = match_token toks Tok_LessEqual in
      let toks, rhs = parse_relationalExpr toks in 
      (toks, Binop(LessEqual, lhs, rhs))
    | _ -> (toks, lhs)
  
  and parse_AdditiveExpr toks = 
    let toks, lhs = parse_MultiplicativeExpr toks in 
    match lookahead toks with 
    | Some Tok_Add ->
      let toks = match_token toks Tok_Add in 
      let toks, rhs = parse_AdditiveExpr toks in 
      (toks, Binop(Add,lhs, rhs))
    | Some Tok_Sub ->
      let toks = match_token toks Tok_Sub in 
      let toks, rhs = parse_AdditiveExpr toks in 
      (toks, Binop(Sub, lhs, rhs))
    | _ -> (toks, lhs)
  
  and parse_MultiplicativeExpr toks = 
    let toks, lhs = parse_ConcatExpr toks in
    match lookahead toks with
    |Some Tok_Mult -> 
      let toks = match_token toks Tok_Mult in
      let toks, rhs = parse_MultiplicativeExpr toks in 
      (toks, Binop(Mult, lhs, rhs))
    |Some Tok_Div -> 
      let toks = match_token toks Tok_Div in 
      let toks, rhs = parse_MultiplicativeExpr toks in
      (toks, Binop(Div, lhs, rhs))
    | _ -> (toks, lhs) 
    
  and parse_ConcatExpr toks = 
    let toks, lhs = parse_UnaryExpr toks in 
    match lookahead toks with 
    | Some Tok_Concat ->
      let toks = match_token toks Tok_Concat in 
      let toks, rhs = parse_ConcatExpr toks in 
      (toks, Binop(Concat, lhs, rhs))
    | _-> (toks,lhs)  
    
    
  and parse_UnaryExpr toks = 
    match lookahead toks with
    | Some Tok_Not -> 
      let toks = match_token toks Tok_Not in 
      let toks, expr = parse_UnaryExpr toks in 
      (toks, Not expr)
    | _ -> parse_FunctionCallExpr toks
    
  and parse_FunctionCallExpr toks = 
    let toks, lhs = parse_primaryExpr toks in 
    match lookahead toks with 
    | Some Tok_LParen | Some (Tok_Int _) | Some (Tok_Bool _) | Some (Tok_String _) | Some (Tok_ID _) ->
      let toks, rhs = parse_primaryExpr toks in 
      (toks, FunctionCall(lhs, rhs))
    | _ -> (toks, lhs)
  
  and parse_primaryExpr toks = 
    match lookahead toks with 
    | Some (Tok_Int i) -> match_token toks (Tok_Int i), Value (Int i)
    | Some (Tok_Bool b) ->  match_token toks (Tok_Bool b), Value (Bool b)
    | Some (Tok_String s) -> match_token toks (Tok_String s), Value (String s)
    | Some (Tok_ID id) -> match_token toks (Tok_ID id), ID id
    | Some (Tok_LParen) ->
      let toks = match_token toks Tok_LParen in 
      let toks, expr = parse_expr toks in 
      let toks = match_token toks Tok_RParen in 
      toks, expr
    | _-> raise (InvalidInputException "unexpected token lol")

     
(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> 
    let toks = match_token toks Tok_Def in 
    (match lookahead toks with 
    | Some(Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in 
      let toks = match_token toks Tok_Equal in
      let toks, expr = parse_expr toks in 
      let toks = match_token toks Tok_DoubleSemi in 
      (toks, Def(id, expr))
    |_-> raise(InvalidInputException "Expected identifier after def"))
  | Some Tok_DoubleSemi ->
    let toks = match_token toks Tok_DoubleSemi in 
    (toks, NoOp)
  | _->
    let toks, expr = parse_expr toks in 
    let toks = match_token toks Tok_DoubleSemi in 
    (toks, Expr(expr))

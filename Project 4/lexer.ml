open TokenTypes


(* Type *)

let rec string_of_list conv lst = 
match lst with
| [] -> ""
| h::[] -> conv h
| h::t -> (conv h) ^ " " ^ (string_of_list conv t)

let tokenize input = 
  let length = String.length input in 
  let rec tok pos = 
    if pos >= length then 
      []
    else if Str.string_match(Str.regexp "[ \t\n]+") input pos then
      tok(pos+(String.length(Str.matched_string input)))

    else if Str.string_match(Str.regexp "[0-9]+") input pos then 
      let expr = Str.matched_string input in 
      Tok_Int(int_of_string expr) :: (tok(pos +String.length expr))

    else if Str.string_match (Str.regexp "(-[0-9]+)") input pos then
      let value = Str.matched_string input in 
      Tok_Int(int_of_string(String.sub value 1 ((String.length value) - 2)))::(tok (pos + String.length value))
    
    else if Str.string_match(Str.regexp "\\btrue\\b\\|\\bfalse\\b") input pos then 
      let expr = Str.matched_string input in 
      Tok_Bool(bool_of_string expr) :: (tok(pos + String.length expr))
 
    else if Str.string_match(Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then  (*Using Type Option' to single out words*)
      let expr = Str.matched_string input in 
      let keyword = match expr with
      |"let" -> Some Tok_Let
      |"rec" -> Some Tok_Rec
      |"in" -> Some Tok_In
      |"def" -> Some Tok_Def
      |"fun" -> Some Tok_Fun
      |"if" -> Some Tok_If
      |"then" -> Some Tok_Then
      |"else" -> Some Tok_Else
      |"not" -> Some Tok_Not
      | _ -> None
      in 
      (match keyword with
      | Some t -> t :: (tok(pos + String.length expr))
      | None -> Tok_ID(expr) :: (tok(pos + String.length expr)))

    else if Str.string_match(Str.regexp "(") input pos then 
      Tok_LParen :: (tok(pos + 1))

    else if Str.string_match(Str.regexp ")") input pos then 
      Tok_RParen :: (tok(pos +1))

    else if Str.string_match(Str.regexp "=") input pos then 
      Tok_Equal :: (tok(pos +2))

    else if Str.string_match(Str.regexp "<>") input pos then 
      Tok_NotEqual :: (tok(pos +2))

    else if Str.string_match(Str.regexp ">=") input pos then 
      Tok_GreaterEqual :: (tok(pos +2))

    else if Str.string_match(Str.regexp "<=") input pos then 
      Tok_LessEqual :: (tok (pos+2))

    else if Str.string_match(Str.regexp ">") input pos then 
      Tok_Greater :: (tok(pos+1))

    else if Str.string_match(Str.regexp "<") input pos then 
      Tok_Less :: (tok(pos + 1))
    
    else if Str.string_match(Str.regexp "||") input pos then 
      Tok_Or :: (tok(pos +2))
    
    else if Str.string_match(Str.regexp "&&") input pos then 
      Tok_And :: (tok(pos +2))
    
    else if Str.string_match(Str.regexp "+") input pos then 
      Tok_Add :: (tok(pos+1))

    else if Str.string_match(Str.regexp "->") input pos then 
      Tok_Arrow :: (tok(pos+2))

    else if Str.string_match(Str.regexp "-") input pos then 
      Tok_Sub :: (tok(pos+1))
    
    else if Str.string_match(Str.regexp "*") input pos then 
      Tok_Mult :: (tok(pos+1))
    
    else if Str.string_match(Str.regexp "/") input pos then 
      Tok_Div :: (tok(pos+1))

    else if Str.string_match(Str.regexp ";;") input pos then 
      Tok_DoubleSemi :: (tok(pos+2))
        
    else if Str.string_match(Str.regexp "\"[^\"]*\"") input pos then 
      let expr = Str.matched_string input in 
      Tok_String(String.sub expr 1 (String.length expr - 2)) :: (tok (pos + String.length expr))

    else if Str.string_match(Str.regexp "\\^") input pos then 
      Tok_Concat :: (tok(pos+1))

    else
      tok(pos + 1)
  in tok 0;; 

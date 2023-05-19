open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)


let rev_tup (a, b, c) = (c, b, a)

let is_even x = x mod 2 = 0

let area (a, b) (x, y) = abs(x-a) * abs(y-b)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = if n < 2 then n else fibonacci(n-1) + fibonacci(n-2)

let rec pow x p = 
  match p with 
  | 0 -> 1 
  | _ -> x * pow x (p-1)


let rec is_prime x = 
  if x <=1 then false
  else  
    let rec is_not_divisor d =
      d*d > x || (x mod d <>0 && is_not_divisor(d+1))
    in
    is_not_divisor 2
  
let rec maxFuncChain init funcs = 
  match funcs with 
  | [] -> init 
  | h :: t -> 
    let x =  maxFuncChain init t in
    let y = h init in
    max x (maxFuncChain y t)

(*****************)
(* Part 3: Lists *)
(*****************)

let reverse lst = 
  let rec helper ls acc = 
    match ls with
    |[] -> acc 
    |h::t -> helper t (h::acc)
  in helper lst []

let rec merge lst1 lst2 = 
  match lst1, lst2 with
  | _,[] -> lst1
  | [],_ -> lst2
  | h :: t, h2 :: t2 -> if h>h2 then h2 :: merge t2 lst1 else h :: merge t lst2

(**)
let rec is_palindrome lst = 
  lst = reverse lst

let jumping_tuples lst1 lst2 = 
  let rec helper index acc lst1 lst2= 
    match lst1, lst2 with 
    | [], _ | _, [] -> (acc, lst1, lst2)
    | (x1, _)::t1, (_, x2)::t2 ->
      if index mod 2 = 0 then 
        helper (index+1) (x2::acc) t1 t2 
      else 
        helper (index+1) (x1::acc) lst1 t2
  in
  let res, _, _ = helper 0 [] lst1 lst2 in
  reverse res

let rec flatten lst = 
  match lst with
  | [] -> []
  | h :: t -> List.append h (flatten t)

let rec square_primes lst =
  match lst with
  | [] -> []
  | h :: t ->
    if is_prime h == true then 
      (h,h * h) :: square_primes t
    else 
      square_primes t

let rec partition p lst = 
  match lst with 
  | [] -> ([],[])
  | h :: t ->
    let (tuple1, tuple2) = partition p t in 
    if p h == true then 
      (h :: tuple1, tuple2)
    else
      (tuple1, h :: tuple2)

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = Funs.map(fun elem -> if elem = x then 1 else 0) lst

let count_occ lst target = Funs.fold_right (fun y acc -> if y = target then acc + 1 else acc) lst 0

let uniq lst = Funs.fold_right (fun x acc -> if count_occ acc x > 0 then acc else x::acc) lst []

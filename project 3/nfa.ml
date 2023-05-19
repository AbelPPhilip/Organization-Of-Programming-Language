open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let rec remove_duplicates lst = 
  match lst with 
  | [] -> [] 
  | h :: t -> h :: (remove_duplicates(List.filter (fun x -> x <>h) t))

let rec contains lst a = 
  match lst with 
  | [] -> false
  | h :: t -> if h = a then true else contains t a

let rec subset lst1 lst2 = 
  match lst1 with 
  | [] -> false
  | h :: t -> if contains lst2 h then true else  subset t lst2

(****************)
(* Part 1: NFAs *)
(****************)

let rec moveHelper (delta: ('q, 's) transition list) (lst: 'q list) (s: 's option) (acc: 'q list) : 'q list=
  match delta with
  | [] -> remove_duplicates acc
  | (a, transition, b) :: t -> 
    if (contains lst a = true && transition = s) then moveHelper t lst s (b :: acc) else moveHelper t lst s acc 

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  match s with 
  | None -> moveHelper nfa.delta qs None []
  | Some sym -> 
      if contains nfa.sigma sym = true then moveHelper nfa.delta qs (Some sym) [] else []

let rec e_closureHelper (nfa : ('q, 's) nfa_t) (qs: 'q list) (visited : 'q list) = 
  let states = move nfa qs None in
  let filteredStates = List.filter(fun x -> not(contains visited x)) states in
  let visited' = visited @ filteredStates in if filteredStates = [] then visited' else e_closureHelper nfa filteredStates visited'

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  e_closureHelper nfa qs qs

let rec acceptHelper (nfa : ('q, char) nfa_t) (lst : char list) (state: 'q list) : bool =
  match lst with
  | [] -> subset (e_closure nfa state) nfa.fs
  | h :: t ->
    let eclosureList = e_closure nfa state in
    let movesList = move nfa eclosureList (Some h) in 
    acceptHelper nfa t movesList


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let str = explode s in 
  acceptHelper nfa str ([nfa.q0] : 'q list)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  List.map (fun symbol -> e_closure nfa (move nfa qs (Some symbol))) nfa.sigma

(*new_states nfa_ex [0] = [[1; 2]]
new_states dfa_ex [0; 1] = [[1]; [0]; [2]]*)

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map(fun symbol -> (qs, Some symbol, e_closure nfa (move nfa qs (Some symbol))))nfa.sigma

  

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if List.exists (fun finalState -> contains qs finalState) nfa.fs then [qs] else []


(*
  visited = []
  q' = e_closure (q,NFA)
  add q' to Q'
  while visited <> Q'
    take unvisited Node -> add to visited 
    for each x in sigma 
      s' = move (s,x, NFA)
      s'' = e_closure (s', NFA)
    if s'' not in Q' 
      add s'' to Q'
    F' = any state in Q' that includes any state in f
*)
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with 
  | [] -> dfa 
  | h :: t ->
    let newStates = new_states nfa h in 
    let newTrans = new_trans nfa h in
    let newFinals = new_finals nfa h in

    let updatedQs = 
      dfa.qs @ newStates @ newFinals 
    in
    let updatedDelta = 
      dfa.delta @ newTrans
    in 
    let updatedFs = 
      dfa.fs @ newFinals 
    in

    let newDFA = {
      sigma = dfa.sigma;
      qs = remove_duplicates updatedQs;
      q0 = dfa.q0;
      fs = remove_duplicates updatedFs;
      delta = updatedDelta; 
    } in  
    let unvisited = List.filter(fun state -> not(contains dfa.qs state)) newStates in 
    let nonDeadStates = List.filter(fun state -> state <> []) unvisited in 
    let newWork = t @ nonDeadStates  in

    nfa_to_dfa_step nfa newDFA newWork
    
(*type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}*) 

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let eclosureStart = e_closure nfa [nfa.q0] in
    let dfa = {
      sigma = nfa.sigma;
      qs = [eclosureStart];
      q0 = eclosureStart;
      delta = [];
      fs = []
    } in 
  nfa_to_dfa_step nfa dfa ([eclosureStart] : 'q list  list)

type person = { name: string;
                age: int;
                hobbies: string list }

type comparator = person -> person -> int

(* Define the type of db below *)
type db = person list

let newDatabase : db = [] 

let insert person db = 
  db @ [person]

let rec remove name db =
  match db with
  | [] -> []
  | {name = n; age =_; hobbies =_} :: t when n = name -> remove name t
  | h::t -> h :: remove name t

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec check condition person = 
    match condition with
    | True -> true 
    | False -> false 
    | Age p -> p person.age
    | Name p -> p person.name 
    | Hobbies p -> p person.hobbies
    | And(x, y) -> check x person && check y person
    | Or (x, y) -> check x person || check y person
    | Not x -> not (check x person)
    | If (x,y,z) -> if check x person then check y person else check z person

let rec query condition db =  
  match db with 
  [] -> []
  | h :: t -> 
    if check condition h then 
      h :: query condition t
    else
      query condition t 
  
let rec sort comparator db = 
  List.sort comparator db

let queryBy condition db comparator = 
  let queriedDb = query condition db in 
  List.sort comparator queriedDb 

let update condition db personData = List.map(fun p -> 
  if check condition p then 
    personData p 
  else p
  ) db

let deleteAll condition db = 
  List.filter(fun person -> not(check condition person)) db

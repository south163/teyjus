(** Maps strings to identifiers. *)

(** A symbol consists of a string and a unique integer identifier. *)
type symbol = string * int

let hashtable = Hashtbl.create 10

let nextsym = ref 0

let symbol name = 
  try 
    let id = Hashtbl.find hashtable name
    in (name, id)
  with
    Not_found -> 
      let id = !nextsym
      in (nextsym := id+1); 
         (Hashtbl.add hashtable name id); 
         (name, id)

let name (n,i) = n

let id (n,i) = i

let currentId = ref 0
  
let gen n =
  let n' = n ^ "_" ^ (string_of_int (!currentId)) in
  let _ = currentId := !currentId + 1 in
  symbol n'


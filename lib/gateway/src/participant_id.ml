open! Core 


type t = int 

let counter = ref 1
let create () = 
  let id = !counter in
  counter := !counter + 1;
  id


let get (arr : 'a Dynarray.t) (t : t) : 'a =
  Dynarray.get arr t


let add (arr : 'a Dynarray.t) (t : t) (data : 'a): unit =
  if t = Dynarray.length arr then Dynarray.add_last arr data
  else ()

let contained_in (arr : 'a Dynarray.t) (t : t) : bool =
  t < (Dynarray.length arr)
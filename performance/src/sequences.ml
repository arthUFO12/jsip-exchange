open! Core

module List_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int list ref

  let transform_index t idx = List.length t - idx - 1
  let create () : int list ref = ref []

  let set t ~key ~data =
    if key = List.length !t
    then t := data :: !t
    else if key > List.length !t || key < 0
    then failwith "key out of range"
    else
      t
      := List.mapi !t ~f:(fun idx original ->
           if idx = transform_index !t key then data else original)
  ;;

  let get t key =
    if key >= List.length !t || key < 0
    then None
    else (
      let idx = transform_index !t key in
      Some (List.nth_exn !t idx))
  ;;
end

module Dynarray_seq = struct
  (* TODO: replace the definition of type t and the implementations of
     create, set, and get *)
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    if key = Dynarray.length t
    then Dynarray.add_last t data
    else if key < 0 || key > Dynarray.length t
    then failwith "key out of range"
    else Dynarray.set t key data
  ;;

  let get t key =
    if key < 0 || key >= Dynarray.length t
    then None
    else Some (Dynarray.get t key)
  ;;
end

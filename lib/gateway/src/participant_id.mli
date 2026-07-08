open! Core

type t

val create : unit -> t
val get : 'a Dynarray.t -> t -> 'a
val add : 'a Dynarray.t -> t -> 'a -> unit
val contained_in : 'a Dynarray.t -> t -> bool

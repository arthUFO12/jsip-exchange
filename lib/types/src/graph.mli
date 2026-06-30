open! Core


type t


val create : unit -> t

val add_node : t -> Symbol.t -> unit

val add_edge : t -> Symbol.t -> Symbol.t -> unit 

val bfs : t -> Symbol.t -> Symbol.t Hash_set.t


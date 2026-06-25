open! Core 


type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Hashable.S with type t := t


val create : unit -> t

val of_int : int -> t
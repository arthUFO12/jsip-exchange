open! Core 


type t [@@deriving sexp, bin_io, compare, equal, hash, string]

include Hashable.S with type t := t

module Generator : sig 
  type gen

  val create : unit -> gen
  val generate : gen -> t
end
val create : unit -> t

val of_int : int -> t
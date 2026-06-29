open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Hashable.Make (T)


let counter = ref 1

let create () =
  let id = !counter in
  counter := !counter + 1;
  id
;;

let of_int = Fn.id
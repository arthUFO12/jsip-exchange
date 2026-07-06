open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Hashable.Make (T)

module Generator = struct
  type gen = { mutable counter : int }

  let create () = { counter = 0 }

  let generate (t : gen) : t =
    let id = t.counter in
    t.counter <- t.counter + 1;
    id
  ;;
end

let counter = ref 1

let create () =
  let id = !counter in
  counter := !counter + 1;
  id
;;

let of_int = Fn.id

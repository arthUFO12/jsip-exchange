open! Core

module T = struct
  type t = string [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

(* Converts string to Symbol.t type Capitalizes if not capitalized already
   Reasoning: all tickers have unique symbols with different characters in
   them so automatic capitalization cannot lead to mixups with different
   tickers *)
let of_string s =
  if String.is_empty s || not (String.for_all s ~f:Char.is_alphanum)
  then
    raise_s
      [%message
        "Symbol.of_string: symbol must be a non-empty alphanumeric string"];
  String.uppercase s
;;

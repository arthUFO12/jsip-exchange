open! Core

type t =
  { max_resting_orders : int
  ; max_submits_per_sec : int
  ; max_cancels_per_sec : int
  }
[@@deriving sexp_of]

(* Named pieces rather than magic numbers on the record, per the project's
   "name constants / break constants into pieces" convention. *)
let default_max_resting_orders = 1000
let default_max_submits_per_sec = 75
let default_max_cancels_per_sec = 75

let default =
  { max_resting_orders = default_max_resting_orders
  ; max_submits_per_sec = default_max_submits_per_sec
  ; max_cancels_per_sec = default_max_cancels_per_sec
  }
;;

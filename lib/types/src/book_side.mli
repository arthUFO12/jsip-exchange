open! Core

type t [@@deriving sexp_of]

val add : t -> Order.t -> unit
val remove : t -> Order_id.t -> Order.t option
val create : Side.t -> t
val best_price : t -> Price.t option
val list_out : t -> Order.t list
val is_empty : t -> bool
val find : t -> Order_id.t -> Order.t option
val find_best_price_time_match : t -> Price.t -> Side.t -> Order.t option
val size : t -> int

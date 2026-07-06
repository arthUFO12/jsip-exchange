open! Core
open Jsip_types

module SymbolConfig : sig
  type t =
    { symbol : Symbol.t
    ; mutable fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; inventory_skew_cents_per_share : int
    }
  [@@deriving sexp_of]
end

type t

(* Configuration for the market maker. *)
module Config : sig
  type cfg =
    { mutable participant : Participant.t
    ; mutable symbol_configs : SymbolConfig.t list
    ; mutable mm_data : t option
    }

  type t = cfg
end

val get_config : t -> Symbol.t -> SymbolConfig.t
val add_request : t -> Order.Request.t -> unit
val apply_fill : t -> Fill.t -> unit
val remove_client_order_id : t -> Symbol.t -> Client_order_id.t -> unit
val compute_effective_inventory : t -> Symbol.t -> Size.t
val get_correlated_symbols : t -> Symbol.t -> Symbol.t list
val display_market_maker_data : t -> unit
val create : Config.t -> t
val get_resting_ids : t -> Symbol.t -> Size.t Client_order_id.Table.t
val get_client_order_ids_list : t -> Symbol.t -> Client_order_id.t list
val get_participant : t -> Participant.t
val get_symbol_list : t -> Symbol.t list
val get_inventory : t -> Symbol.t -> Size.t
val update_correlation : t -> Symbol.t -> Symbol.t -> float -> unit

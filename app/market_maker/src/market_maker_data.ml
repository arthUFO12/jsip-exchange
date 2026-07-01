open! Core
open! Async
open Jsip_types

module SymbolConfig = struct
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



module Correlation = struct
  type t = float [@@deriving sexp, compare, equal]

  let of_float (f : float) =
    if Float.( > ) f 1.0
    then failwith "invalid correlation"
    else if Float.( < ) f (-1.0)
    then failwith "invalid correlation"
    else f
  ;;
end

(* Market_maker_data module: used for keeping track of the client order ids
   on the books and inventory that the market maker has *)

type symbol_data =
  { mutable inventory : Size.t
  ; resting_ids : Size.t Client_order_id.Table.t
  ; symbol_config : SymbolConfig.t
  ; idx : int
  }

let create_symbol_data idx symbol_config =
  { inventory = Size.zero
  ; resting_ids = Client_order_id.Table.create ()
  ; symbol_config
  ; idx
  }
;;

type t =
  { per_symbol_data : symbol_data Symbol.Table.t
  ; participant : Participant.t
  ; correlation_matrix : Correlation.t array array
  ; symbol_array : Symbol.t array
  }
  (* Configuration for the market maker. *)
module Config = struct
  type cfg =
    { mutable participant : Participant.t
    ; mutable symbol_configs : SymbolConfig.t list
    ; mutable mm_data : t option
    }

  type t = cfg

  
  let get_symbols t = List.map t.symbol_configs ~f:(fun cfg -> cfg.symbol)
end


let create (config : Config.t) =
  let symbol_configs = config.symbol_configs in
  let symbol_array = Config.get_symbols config |> List.to_array in
  let num_symbols = List.length symbol_configs in
  let correlation_matrix =
    Array.make_matrix ~dimx:num_symbols ~dimy:num_symbols 0.0
  in
  let per_symbol_data = Symbol.Table.create () in
  List.iteri symbol_configs ~f:(fun idx cfg ->
    Hashtbl.set
      per_symbol_data
      ~key:cfg.symbol
      ~data:(create_symbol_data idx cfg);
    correlation_matrix.(idx).(idx) <- 1.0);
  { per_symbol_data
  ; participant = config.participant
  ; correlation_matrix
  ; symbol_array
  }
;;

let get_resting_ids t symbol =
  (Hashtbl.find_exn t.per_symbol_data symbol).resting_ids
;;

let get_inventory t symbol =
  (Hashtbl.find_exn t.per_symbol_data symbol).inventory
;;

let get_symbol_data t symbol = Hashtbl.find_exn t.per_symbol_data symbol

(* Updates the Market_maker_data.t after the client has received an
   Order_accept event *)
let add_request t (request : Order.Request.t) =
  let client_order_ids = get_resting_ids t request.symbol in
  Hashtbl.set
    client_order_ids
    ~key:request.client_order_id
    ~data:request.size
;;

(* Updates the Market_maker_data.t after receiving an Order_cancel event *)
let remove_client_order_id t symbol client_order_id =
  let client_order_id_set = get_resting_ids t symbol in
  Hashtbl.remove client_order_id_set client_order_id
;;

(* Updates the Market_maker_data.t after receiving a Fill event *)
let apply_fill t (fill : Fill.t) =
  let fill_size = fill.size in
  let symbol = fill.symbol in
  (* Match the side and coid based on whether the aggressor or resting name
     matches the market maker's name *)
  let side, client_order_id =
    if Participant.equal t.participant fill.aggressor_participant
    then fill.aggressor_side, fill.aggressor_client_order_id
    else if Participant.equal t.participant fill.resting_participant
    then Side.flip fill.aggressor_side, fill.resting_client_order_id
    else failwith "fill does not apply to this market maker"
  in
  let symbol_resting_ids = get_resting_ids t symbol in
  let order_size = Hashtbl.find_exn symbol_resting_ids client_order_id in
  let symbol_data = get_symbol_data t symbol in
  let inventory_size = symbol_data.inventory in
  (* if order is a buy, inventory grows, if it is a sell, inventory shrinks *)
  let new_inventory_size =
    match (side : Side.t) with
    | Buy -> Size.( + ) inventory_size fill_size
    | Sell -> Size.( - ) inventory_size fill_size
  in
  let new_order_size = Size.( - ) order_size fill_size in
  symbol_data.inventory <- new_inventory_size;
  (* remove the order from our resting order ids if the size is now zero,
     otherwise update it with the new order size *)
  if Size.equal new_order_size Size.zero
  then Hashtbl.remove symbol_resting_ids client_order_id
  else if Size.( < ) new_order_size Size.zero
  then failwith "accounting is wrong. negative order size"
  else
    Hashtbl.set symbol_resting_ids ~key:client_order_id ~data:new_order_size
;;

let get_idx t symbol = (Hashtbl.find_exn t.per_symbol_data symbol).idx
let get_symbol t idx = t.symbol_array.(idx)

let get_config t symbol =
  (Hashtbl.find_exn t.per_symbol_data symbol).symbol_config
;;

let compute_effective_inventory t symbol =
  let size_to_float s = s |> Size.to_int |> Float.of_int in
  let float_to_size f = f |> Int.of_float |> Size.of_int in
  let computed_symbol_idx = get_idx t symbol in
  Hashtbl.fold
    t.per_symbol_data
    ~init:0.0
    ~f:(fun ~key:_ ~data:other_symbol_data sum ->
      let other_symbol_inventory_float =
        other_symbol_data.inventory |> size_to_float
      in
      let correlation =
        t.correlation_matrix.(computed_symbol_idx).(other_symbol_data.idx)
      in
      sum +. (correlation *. other_symbol_inventory_float))
  |> float_to_size
;;

let update_correlation t first second corr =
  let parsed_corr = Correlation.of_float corr in
  if Symbol.equal first second
  then ()
  else (
    let first_idx = get_idx t first in
    let second_idx = get_idx t second in
    t.correlation_matrix.(first_idx).(second_idx) <- parsed_corr;
    t.correlation_matrix.(second_idx).(first_idx) <- parsed_corr)
;;

let get_correlated_symbols t symbol =
  let symbol_idx = get_idx t symbol in
  Array.filter_mapi t.correlation_matrix.(symbol_idx) ~f:(fun idx corr ->
    if Float.( = ) corr 0.0 then None else Some idx)
  |> Array.map ~f:(fun idx -> get_symbol t idx)
  |> Array.to_list
;;

let get_client_order_ids_list t symbol =
  get_resting_ids t symbol |> Hashtbl.to_alist |> List.map ~f:fst
;;

let get_participant t = t.participant

let _get_client_order_ids_set t symbol =
  get_client_order_ids_list t symbol
  |> Hash_set.of_list (module Client_order_id)
;;

let display_market_maker_data t =
  Hashtbl.iteri t.per_symbol_data ~f:(fun ~key:symbol ~data ->
    print_endline
      [%string
        "symbol: %{symbol#Symbol} inventory_size: %{data.inventory#Size}"]);
  Hashtbl.iteri t.per_symbol_data ~f:(fun ~key:symbol ~data ->
    print_endline [%string "symbol: %{symbol#Symbol}"];
    Hashtbl.iteri data.resting_ids ~f:(fun ~key:coid ~data:size ->
      print_endline
        [%string
          "client_order_id: %{coid#Client_order_id} order_size: %{size#Size}"]);
    print_endline "")
;;

let get_symbol_list t = Hashtbl.to_alist t.per_symbol_data |> List.map ~f:fst

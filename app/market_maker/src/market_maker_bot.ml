open! Core
open! Async
open Jsip_bot_runtime
open Jsip_types

module Context = Bot_runtime.Context
let initialize_config_from_context 
  (config : Market_maker_data.Config.t) 
  (context : Bot_runtime.Context.t) 
  =
  List.iter config.symbol_configs ~f:(fun symbol_config -> symbol_config.fair_value_cents <- (Context.fundamental context symbol_config.symbol |> Price.to_int_cents));
  config.participant <- (Context.participant context);
  let mm_data = Market_maker_data.create config in
  config.mm_data <- Some mm_data;;

let get_mm_data mm_data = Option.value_exn mm_data
;;
module T : Jsip_bot_runtime.Bot_runtime.Bot = struct
  module Config = struct
    type t = Market_maker_data.Config.t
  end

  let on_start (config : Config.t) (context : Bot_runtime.Context.t) =
    initialize_config_from_context config context;
    let symbol_list = Market_maker_data.get_symbol_list (get_mm_data config.mm_data) in
    Deferred.List.iter symbol_list ~how:`Parallel ~f:(fun symbol ->
      let cfg = Market_maker_data.get_config (get_mm_data config.mm_data) symbol in
      let sub order = Bot_runtime.Context.submit context order >>| Or_error.ok_exn in
      Market_maker.seed_book (get_mm_data config.mm_data) symbol cfg.fair_value_cents sub)
  ;;

  let on_tick (config : Config.t) (context : Bot_runtime.Context.t) =
    ignore config;
    ignore context;
    return ()
  ;;

  let on_event (config : Config.t) (context : Bot_runtime.Context.t) (event : Exchange_event.t) =
    Market_maker.update_market_maker_data (get_mm_data config.mm_data) event;
    let submit order = Bot_runtime.Context.submit context order >>| Or_error.ok_exn in
    let cancel coid = Bot_runtime.Context.cancel context coid >>| Or_error.ok_exn in
    return (Market_maker.repost_after_fill (get_mm_data config.mm_data) event submit cancel)
  ;;

  let name = "MarketMaker"
end

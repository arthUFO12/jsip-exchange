open! Core
open! Async
open Jsip_types
open Jsip_gateway

(* Config Module: Used for deriving the pricing of a symbol the market maker
   is trading on *)

(* helper for submitting order requests *)

(* helper for submitting order cancels. We ignore errors here *)

(* Clears the book of all resting orders *)
let clear_book (mm_data : Market_maker_data.t) symbol cancel =
  let client_order_id_list =
    Market_maker_data.get_client_order_ids_list mm_data symbol
  in
  Deferred.List.iter ~how:`Parallel client_order_id_list ~f:cancel
;;

(* computes the skewed fair value based on the config and current inventory *)
let compute_skewed_fair_cents mm_data symbol inventory =
  let config = Market_maker_data.get_config mm_data symbol in
  config.fair_value_cents
  - (Size.to_int inventory * config.inventory_skew_cents_per_share)
;;

(* seeds a one side of the book *)
let seed_book_side mm_data symbol side fair_value_cents submit =
  (* if we are buying, we offset prices downwards and away from the fair
     value, if we are selling we offset them upwards *)
  let config = Market_maker_data.get_config mm_data symbol in
  let add_or_sub =
    match (side : Side.t) with Buy -> Int.( - ) | Sell -> Int.( + )
  in
  (* send a submit request for each level *)
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind () =
        submit
          ({ symbol = config.symbol
           ; side
           ; price = Price.of_int_cents (add_or_sub fair_value_cents offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.create ()
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

(* seeds both sides of the book *)
let seed_book mm_data symbol fair_value_cents conn =
  (* if skewed fair value is given, then use it otherwise use default fair
     value *)

  (* wait until both buy and sell sides finish *)
  Deferred.all_unit
    [ seed_book_side mm_data symbol Buy fair_value_cents conn
    ; seed_book_side mm_data symbol Sell fair_value_cents conn
    ]
;;

(* Updates market maker data after receiving an event *)
let update_market_maker_data
  (market_maker_data : Market_maker_data.t)
  (event : Exchange_event.t)
  =
  match (event : Exchange_event.t) with
  (* In the order accept case we just add the order to our list of resting
     COIDs *)
  | Exchange_event.Order_accept { order_id = _; request; _ } ->
    Market_maker_data.add_request market_maker_data request
  (* In the fill case we *)
  | Fill fill -> Market_maker_data.apply_fill market_maker_data fill
  | Order_cancel cancel ->
    Market_maker_data.remove_client_order_id
      market_maker_data
      cancel.symbol
      cancel.client_order_id
  | Cancel_reject _ | Order_reject _ | Best_bid_offer_update _
  | Trade_report _ ->
    ()
;;

let repost_symbol mm_data submit cancel symbol =
  let _ = clear_book mm_data symbol cancel in
  let fair_value_cents =
    Market_maker_data.compute_effective_inventory mm_data symbol
    |> compute_skewed_fair_cents mm_data symbol
  in
  let _ = seed_book mm_data symbol fair_value_cents submit in
  ()
;;

(* clears book and reposts orders based on new inventory *)
let repost_after_fill mm_data event submit cancel =
  match (event : Exchange_event.t) with
  | Fill f ->
    let correlated_symbols =
      Market_maker_data.get_correlated_symbols mm_data f.symbol
    in
    List.iter correlated_symbols ~f:(repost_symbol mm_data submit cancel)
  | _ -> ()
;;

(* displays the market maker data *)

let run'
  ~testing
  (config : Market_maker_data.Config.t)
  (conn : Rpc.Connection.t)
  =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  let cancel client_order_id =
    let%map _ =
      Rpc.Rpc.dispatch_exn Rpc_protocol.cancel_order_rpc conn client_order_id
    in
    ()
  in
  let%bind _ =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string config.participant)
    >>| Or_error.ok_exn
  in
  let mm_data = Market_maker_data.create config in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       update_market_maker_data mm_data event;
       repost_after_fill mm_data event submit cancel;
       if testing
       then (
         let e = Protocol.format_event event in
         print_endline [%string "%{e}\n"];
         Market_maker_data.display_market_maker_data mm_data)));
  let symbol_list = Market_maker_data.get_symbol_list mm_data in
  Deferred.List.iter symbol_list ~how:`Parallel ~f:(fun symbol ->
    let cfg = Market_maker_data.get_config mm_data symbol in
    seed_book mm_data symbol cfg.fair_value_cents submit)
;;

let run config conn = run' ~testing:false config conn

module For_testing = struct
  let run config conn = run' ~testing:true config conn
end

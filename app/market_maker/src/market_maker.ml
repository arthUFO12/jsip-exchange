open! Core
open! Async
open Jsip_types
open Jsip_gateway

(* Config Module: Used for deriving the pricing of a symbol the market maker
   is trading on *)
module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; inventory_skew_cents_per_share : int
    }
  [@@deriving sexp_of]
end

(* Market_maker_data module: used for keeping track of the client order ids
   on the books and inventory that the market maker has *)
module Market_maker_data = struct
  type t =
    { inventory : Size.t Symbol.Table.t
    ; resting_ids : Size.t Client_order_id.Table.t Symbol.Table.t
    ; participant : Participant.t
    }

  let create ?participant () =
    { inventory = Symbol.Table.create ()
    ; resting_ids = Symbol.Table.create ()
    ; participant =
        Option.value
          participant
          ~default:(Participant.of_string "MarketMaker")
    }
  ;;

  (* Updates the Market_maker_data.t after the client has received an
     Order_accept event *)
  let add_request t (request : Order.Request.t) =
    let client_order_ids =
      Hashtbl.find_or_add
        t.resting_ids
        request.symbol
        ~default:Client_order_id.Table.create
    in
    Hashtbl.set
      client_order_ids
      ~key:request.client_order_id
      ~data:request.size
  ;;

  (* Updates the Market_maker_data.t after receiving an Order_cancel event *)
  let remove_client_order_id t symbol client_order_id =
    let client_order_id_set = Hashtbl.find_exn t.resting_ids symbol in
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
    let symbol_resting_ids = Hashtbl.find_exn t.resting_ids symbol in
    let order_size = Hashtbl.find_exn symbol_resting_ids client_order_id in
    let inventory_size =
      Hashtbl.find_or_add t.inventory symbol ~default:(fun () -> Size.zero)
    in
    (* if order is a buy, inventory grows, if it is a sell, inventory shrinks *)
    let new_inventory_size =
      match (side : Side.t) with
      | Buy -> Size.( + ) inventory_size fill_size
      | Sell -> Size.( - ) inventory_size fill_size
    in
    let new_order_size = Size.( - ) order_size fill_size in
    Hashtbl.set t.inventory ~key:symbol ~data:new_inventory_size;
    (* remove the order from our resting order ids if the size is now zero,
       otherwise update it with the new order size *)
    if Size.equal new_order_size Size.zero
    then Hashtbl.remove symbol_resting_ids client_order_id
    else if Size.( < ) new_order_size Size.zero
    then failwith "accounting is wrong. negative order size"
    else
      Hashtbl.set
        symbol_resting_ids
        ~key:client_order_id
        ~data:new_order_size
  ;;

  let get_symbol_inventory t symbol = Hashtbl.find_exn t.inventory symbol

  let get_client_order_ids_list t symbol =
    Hashtbl.find_exn t.resting_ids symbol
    |> Hashtbl.to_alist
    |> List.map ~f:fst
  ;;

  let _get_client_order_ids_set t symbol =
    get_client_order_ids_list t symbol
    |> Hash_set.of_list (module Client_order_id)
  ;;
end

(* helper for submitting order requests *)
let submit conn request =
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
;;

(* helper for submitting order cancels. We ignore errors here *)
let cancel conn client_order_id =
  let%map _ =
    Rpc.Rpc.dispatch_exn Rpc_protocol.cancel_order_rpc conn client_order_id
  in
  ()
;;

(* Clears the book of all resting orders *)
let clear_book (mm_data : Market_maker_data.t) (config : Config.t) conn =
  let client_order_id_list =
    Market_maker_data.get_client_order_ids_list mm_data config.symbol
  in
  Deferred.List.iter ~how:`Parallel client_order_id_list ~f:(cancel conn)
;;

(* computes the skewed fair value based on the config and current inventory *)
let compute_skewed_fair_cents (config : Config.t) inventory =
  config.fair_value_cents
  - (Size.to_int inventory * config.inventory_skew_cents_per_share)
;;

(* seeds a one side of the book *)
let seed_book_side side (config : Config.t) fair_value_cents conn =
  (* if we are buying, we offset prices downwards and away from the fair
     value, if we are selling we offset them upwards *)
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
          conn
          ({ symbol = config.symbol
           ; participant = config.participant
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
let seed_book ?fair_value_cents (config : Config.t) conn =
  (* if skewed fair value is given, then use it otherwise use default fair
     value *)
  let fvc =
    match fair_value_cents with
    | None -> config.fair_value_cents
    | Some fvc -> fvc
  in
  (* wait until both buy and sell sides finish *)
  Deferred.all_unit
    [ seed_book_side Buy config fvc conn
    ; seed_book_side Sell config fvc conn
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
  | Exchange_event.Order_accept { order_id = _; request } ->
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

(* clears book and reposts orders based on new inventory *)
let repost_after_fill do_repost market_maker_data config conn event =
  if do_repost
  then (
    match (event : Exchange_event.t) with
    | Fill _ ->
      let _ = clear_book market_maker_data config conn in
      let fair_value_cents =
        Market_maker_data.get_symbol_inventory
          market_maker_data
          config.symbol
        |> compute_skewed_fair_cents config
      in
      let _ = seed_book ~fair_value_cents config conn in
      ()
    | _ -> ())
;;

(* displays the market maker data *)
let display_market_maker_data (market_maker_data : Market_maker_data.t) =
  Hashtbl.iteri market_maker_data.inventory ~f:(fun ~key:symbol ~data:size ->
    print_endline
      [%string "symbol: %{symbol#Symbol} inventory_size: %{size#Size}"]);
  Hashtbl.iteri
    market_maker_data.resting_ids
    ~f:(fun ~key:symbol ~data:client_order_id_table ->
      print_endline [%string "symbol: %{symbol#Symbol}"];
      Hashtbl.iteri client_order_id_table ~f:(fun ~key:coid ~data:size ->
        print_endline
          [%string
            "client_order_id: %{coid#Client_order_id} order_size: \
             %{size#Size}"]);
      print_endline "")
;;

let run' ~testing ~do_repost (config : Config.t) (conn : Rpc.Connection.t) =
  let%bind participant =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string config.participant)
    >>| Or_error.ok_exn
  in
  let market_maker_data = Market_maker_data.create ~participant () in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       update_market_maker_data market_maker_data event;
       repost_after_fill do_repost market_maker_data config conn event;
       if testing
       then (
         let e = Protocol.format_event event in
         print_endline [%string "%{e}\n"];
         display_market_maker_data market_maker_data)));
  seed_book config conn
;;

let run config conn = run' ~testing:false ~do_repost:true config conn

module For_testing = struct
  let run ~do_repost config conn = run' ~testing:true ~do_repost config conn
end

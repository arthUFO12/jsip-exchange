open! Core
open Jsip_types
open Async_log_kernel.Ppx_log_syntax

module IntMap = Map.Make(Int)


module BookSide = struct
  type t =
    { side : Side.t
    ; mutable price_levels : Order.t Queue.t Price.Map.t
    ; id_to_price : (Order_id.t, Price.t) Hashtbl_intf.Hashtbl.t
    }

    
  let _add t order =
    let order_price = Order.price order in
    let order_id = Order.order_id order in
    let order_queue =
      match Map.find t.price_levels order_price with 
        | None -> t.price_levels <- (match Map.add t.price_levels ~key:order_price ~data:(Queue.create ()) with 
        | `Duplicate -> t.price_levels
        | `Ok map -> map); Map.find_exn t.price_levels order_price
        | Some queue -> queue
    in
    Hashtbl.add_exn t.id_to_price ~key:order_id ~data:order_price;
    Queue.enqueue order_queue order;
  ;;

  let _clean_up_price_level t (price : Price.t) =
    match Map.find_exn t.price_levels price |> Queue.is_empty with
    | true ->
      let _ = Map.remove t.price_levels price in ();
    | false -> ()
  ;;

  let _remove t order_id =
    let order_price = Hashtbl.find_exn t.id_to_price order_id in
    let price_queue = Map.find_exn t.price_levels order_price in
    let front_id = Queue.peek_exn price_queue |> Order.order_id in
    if Order_id.equal order_id front_id
    then
      Queue.dequeue_and_ignore_exn price_queue
    else
      Queue.filter_inplace price_queue ~f:(fun ele ->
        let ele_id = Order.order_id ele in
        not (Order_id.equal order_id ele_id));
    _clean_up_price_level t order_price
  ;;

  let _create side =
    let map = Price.Map.empty in
    let id_to_price =
      Hashtbl.create ~growth_allowed:true (module Order_id)
    in
    { side; price_levels = map;  id_to_price }
  ;;

  let _best_price t = match Map.max_elt t.price_levels with 
    | None -> None
    | Some (price, _) -> Some price

  let _list_out t =
    let list = ref [] in
    Map.iter t.price_levels ~f:(fun q ->
      list := !list @ Queue.to_list q)


  let _find t order_id =
    match Hashtbl.find t.id_to_price order_id with 
      | None -> None
      | Some price -> 
        Map.find_exn t.price_levels price |> Queue.find ~f:(fun ord -> Order_id.equal (Order.order_id ord) order_id)

end

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t list
  ; mutable asks : Order.t list
  }
[@@deriving sexp_of]

let create symbol = { symbol; bids = []; asks = [] }
let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_list t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let add t order =
  let side = Order.side order in
  set_side_list t side (order :: side_list t side)
;;

let remove' t order_id =
  let remove_from t side order_id =
    let orders = side_list t side in
    match
      List.partition_tf orders ~f:(fun o ->
        Order_id.equal (Order.order_id o) order_id)
    with
    | [], _ -> None
    | [ found ], rest ->
      set_side_list t side rest;
      Some found
    | matches, _ ->
      [%log.info
        "BUG: More than one order matching order_id found when removing"
          (order_id : Order_id.t)
          (matches : Order.t list)
          (t.symbol : Symbol.t)
          (side : Side.t)];
      None
  in
  match remove_from t Buy order_id with
  | Some _ as result -> result
  | None -> remove_from t Sell order_id
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  let find_in side =
    List.find (side_list t side) ~f:(fun o ->
      Order_id.equal (Order.order_id o) order_id)
  in
  match find_in Buy with Some _ as result -> result | None -> find_in Sell
;;

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)

let find_match t incoming =
  let incoming_side = Order.side incoming in
  let incoming_price = Order.price incoming in
  let opposite_side = Side.flip incoming_side in
  let resting_orders = side_list t opposite_side in
  let marketable_orders =
    List.filter resting_orders ~f:(fun order ->
      Price.is_marketable
        incoming_side
        ~price:incoming_price
        ~resting_price:(Order.price order))
  in
  List.reduce marketable_orders ~f:(fun ord1 ord2 ->
    if Order.better_price_time opposite_side ~ord1 ~ord2 then ord1 else ord2)
;;

let orders_on_side t side = side_list t side
let is_empty t = List.is_empty t.bids && List.is_empty t.asks
let count t side = List.length (side_list t side)

let best_price t side =
  side_list t side
  |> List.map ~f:Order.price
  |> List.reduce ~f:(fun price1 price2 ->
    if Price.is_more_aggressive side ~price:price1 ~than:price2
    then price1
    else price2)
;;

let best_level t side : Level.t option =
  match best_price t side with
  | None -> None
  | Some price ->
    let total_size =
      List.fold (side_list t side) ~init:Size.zero ~f:(fun acc order ->
        if Price.equal (Order.price order) price
        then Size.( + ) acc (Order.remaining_size order)
        else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

let snapshot_side t (side : Side.t) =
  orders_on_side t side
  |> List.sort ~compare:(Comparable.reverse (Order.price_time_cmp side))
  |> List.map ~f:Level.of_order
;;

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end

open! Core
open Jsip_types
module IntMap = Map.Make (Int)

module BookSide = struct
  type t =
    { side : Side.t
    ; mutable price_levels : Order.t Queue.t Price.Map.t
    ; id_to_price : (Order_id.t, Price.t) Hashtbl_intf.Hashtbl.t
    }
  [@@deriving sexp_of]

  let add t order =
    let order_price = Order.price order in
    let order_id = Order.order_id order in
    let order_queue =
      match Map.find t.price_levels order_price with
      | None ->
        t.price_levels
        <- (match
              Map.add t.price_levels ~key:order_price ~data:(Queue.create ())
            with
            | `Duplicate -> t.price_levels
            | `Ok map -> map);
        Map.find_exn t.price_levels order_price
      | Some queue -> queue
    in
    Hashtbl.add_exn t.id_to_price ~key:order_id ~data:order_price;
    Queue.enqueue order_queue order
  ;;

  let clean_up_price_level t (price : Price.t) =
    let price_queue = Map.find_exn t.price_levels price in
    if Queue.is_empty price_queue
    then t.price_levels <- Map.remove t.price_levels price
  ;;

  let remove t order_id =
    match Hashtbl.find t.id_to_price order_id with
    | None -> None
    | Some order_price ->
      let price_queue = Map.find_exn t.price_levels order_price in
      let front_id = Queue.peek_exn price_queue |> Order.order_id in
      let order =
        if Order_id.equal order_id front_id
        then Queue.dequeue price_queue
        else (
          let equal_func x = Order_id.equal (Order.order_id x) order_id in
          let found_order = Queue.find price_queue ~f:equal_func in
          Queue.filter_inplace price_queue ~f:(fun ele ->
            not (equal_func ele));
          found_order)
      in
      Hashtbl.remove t.id_to_price order_id;
      clean_up_price_level t order_price;
      order
  ;;

  let create side =
    let map = Price.Map.empty in
    let id_to_price =
      Hashtbl.create ~growth_allowed:true (module Order_id)
    in
    { side; price_levels = map; id_to_price }
  ;;

  let best_price t =
    let best_price =
      match (t.side : Side.t) with
      | Buy -> Map.max_elt t.price_levels
      | Sell -> Map.min_elt t.price_levels
    in
    match best_price with None -> None | Some (price, _) -> Some price
  ;;

  let list_out t =
    let list = ref [] in
    Map.iter t.price_levels ~f:(fun q ->
      list
      := match (t.side : Side.t) with
         | Buy -> Queue.to_list q @ !list
         | Sell -> !list @ Queue.to_list q);
    !list
  ;;

  let is_empty t = Map.is_empty t.price_levels

  let size t =
    Map.fold t.price_levels ~init:0 ~f:(fun ~key:_ ~data sum ->
      sum + Queue.length data)
  ;;

  let find t order_id =
    match Hashtbl.find t.id_to_price order_id with
    | None -> None
    | Some order_price ->
      let price_queue = Map.find_exn t.price_levels order_price in
      Queue.find price_queue ~f:(fun x ->
        Order_id.equal (Order.order_id x) order_id)
  ;;

  let find_best_price_time_match t price buy_or_sell =
    match (buy_or_sell : Side.t) with
    | Buy ->
      (match Map.min_elt t.price_levels with
       | None -> None
       | Some (key, queue) when Price.( <= ) key price -> Queue.peek queue
       | Some _ -> None)
    | Sell ->
      (match Map.max_elt t.price_levels with
       | None -> None
       | Some (key, queue) when Price.( >= ) key price -> Queue.peek queue
       | Some _ -> None)
  ;;
end

type t =
  { symbol : Symbol.t
  ; bids : BookSide.t
  ; asks : BookSide.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = BookSide.create Side.Buy
  ; asks = BookSide.create Side.Sell
  }
;;

let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with
  | Buy -> BookSide.list_out t.bids
  | Sell -> BookSide.list_out t.asks
;;

let add t order =
  match (Order.side order : Side.t) with
  | Buy -> BookSide.add t.bids order
  | Sell -> BookSide.add t.asks order
;;

let remove' t order_id =
  match BookSide.remove t.bids order_id with
  | Some result -> Some result
  | None -> BookSide.remove t.asks order_id
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  match BookSide.find t.bids order_id with
  | Some _ as result -> result
  | None -> BookSide.find t.asks order_id
;;

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)

let find_match t incoming =
  let incoming_side = Order.side incoming in
  let incoming_price = Order.price incoming in
  let opposite_bookside =
    match incoming_side with Buy -> t.asks | Sell -> t.bids
  in
  BookSide.find_best_price_time_match
    opposite_bookside
    incoming_price
    incoming_side
;;

let orders_on_side t side = side_list t side
let is_empty t = BookSide.is_empty t.bids && BookSide.is_empty t.asks

let count t side =
  match (side : Side.t) with
  | Buy -> BookSide.size t.bids
  | Sell -> BookSide.size t.asks
;;

let best_price t side =
  match (side : Side.t) with
  | Buy -> BookSide.best_price t.bids
  | Sell -> BookSide.best_price t.asks
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
  (match side with
   | Buy -> BookSide.list_out t.bids
   | Sell -> BookSide.list_out t.asks)
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

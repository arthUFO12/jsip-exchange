open! Core
open Jsip_types
module IntMap = Map.Make (Int)



type t =
  { symbol : Symbol.t
  ; bids : Book_side.t
  ; asks : Book_side.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Book_side.create Side.Buy
  ; asks = Book_side.create Side.Sell
  }
;;

let symbol t = t.symbol

let side_list t side =
  match (side : Side.t) with
  | Buy -> Book_side.list_out t.bids
  | Sell -> Book_side.list_out t.asks
;;

let add t order =
  match (Order.side order : Side.t) with
  | Buy -> Book_side.add t.bids order
  | Sell -> Book_side.add t.asks order
;;

let remove' t order_id =
  match Book_side.remove t.bids order_id with
  | Some result -> Some result
  | None -> Book_side.remove t.asks order_id
;;

let remove t order_id = ignore (remove' t order_id : Order.t option)

let find t order_id =
  match Book_side.find t.bids order_id with
  | Some _ as result -> result
  | None -> Book_side.find t.asks order_id
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
  Book_side.find_best_price_time_match
    opposite_bookside
    incoming_price
    incoming_side
;;

let orders_on_side t side = side_list t side
let is_empty t = Book_side.is_empty t.bids && Book_side.is_empty t.asks

let count t side =
  match (side : Side.t) with
  | Buy -> Book_side.size t.bids
  | Sell -> Book_side.size t.asks
;;

let best_price t side =
  match (side : Side.t) with
  | Buy -> Book_side.best_price t.bids
  | Sell -> Book_side.best_price t.asks
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
   | Buy -> Book_side.list_out t.bids
   | Sell -> Book_side.list_out t.asks)
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

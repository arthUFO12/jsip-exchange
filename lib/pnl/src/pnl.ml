open! Core
open Jsip_types

(* The state we keep for one [(participant, symbol)] pair.

   [inventory] is signed: positive for a long, negative for a short.
   [cost_basis_cents] is the signed total cost of the {e currently open}
   position, so that [cost_basis_cents = inventory * average_entry_price]
   holds by construction. For a long you paid cash (positive basis); for a
   short you received cash (negative basis). [realized_cents] accumulates
   cash booked by closing positions. *)
type position =
  { inventory : int
  ; cost_basis_cents : int
  ; realized_cents : int
  }
[@@deriving sexp_of]

let flat_position =
  { inventory = 0; cost_basis_cents = 0; realized_cents = 0 }
;;

type t =
  { positions : position Symbol.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol.Map.empty }
;;

(* [realized_on_reduce] computes the realized cash, in cents, produced when a
   trade closes [closed] shares of an existing position.

   Inputs:
   - [closed]: how many shares of the open position are being closed (>= 0).
   - [trade_price_cents]: the price of the closing trade, in cents.
   - [cost_basis_removed_cents]: the portion of the position's cost basis
     that these [closed] shares represent. Signed like the cost basis
     (positive when closing a long, negative when closing a short).
   - [position_side]: [Buy] if the position being reduced is long, [Sell] if
     it is short. [Side.sign] gives [+1] / [-1].

   TODO(human): return the realized cents. *)
let realized_on_reduce
  ~closed
  ~trade_price_cents
  ~cost_basis_removed_cents
  ~(position_side : Side.t)
  =
  let shares_closed =
    match position_side with Buy -> closed | Sell -> Int.neg closed
  in
  (shares_closed * trade_price_cents) - cost_basis_removed_cents
;;

(* Apply a single trade of [dq] signed shares (positive = bought, negative =
   sold) at [price_cents] to one position, returning the updated position. *)
let apply_trade position ~dq ~price_cents =
  let { inventory; cost_basis_cents; realized_cents } = position in
  match inventory = 0 || Sign.equal (Int.sign inventory) (Int.sign dq) with
  | true ->
    (* Opening a new position or adding to the existing one: no cash is
       realized, and the trade's cost rolls straight into the basis. *)
    { position with
      inventory = inventory + dq
    ; cost_basis_cents = cost_basis_cents + (dq * price_cents)
    }
  | false ->
    (* The trade reduces, closes, or flips the position. *)
    let closed = Int.min (abs dq) (abs inventory) in
    let position_side : Side.t = if inventory > 0 then Buy else Sell in
    (* The share of the cost basis attributable to the closed shares. *)
    let cost_basis_removed = cost_basis_cents * (closed / abs inventory) in
    let realized_delta =
      realized_on_reduce
        ~closed
        ~trade_price_cents:price_cents
        ~cost_basis_removed_cents:cost_basis_removed
        ~position_side
    in
    let realized_cents = realized_cents + realized_delta in
    (match abs dq <= abs inventory with
     | true ->
       (* Reduce or exactly close: the remaining shares keep their average
          entry price, so we just drop the closed portion of the basis. *)
       { inventory = inventory + dq
       ; cost_basis_cents = cost_basis_cents - cost_basis_removed
       ; realized_cents
       }
     | false ->
       (* Flip: close the whole old position, then open the leftover on the
          new side at the trade price. *)
       let remainder = inventory + dq in
       { inventory = remainder
       ; cost_basis_cents = remainder * price_cents
       ; realized_cents
       })
;;

let update_position t ~participant ~symbol ~(side : Side.t) ~size ~price =
  let dq = Side.sign side * size in
  let price_cents = Price.to_int_cents price in
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let position =
    Map.find by_symbol symbol
    |> Option.value ~default:flat_position
    |> apply_trade ~dq ~price_cents
  in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let apply_fill t (fill : Fill.t) =
  let size = Size.to_int fill.size in
  update_position
    t
    ~participant:fill.aggressor_participant
    ~symbol:fill.symbol
    ~side:fill.aggressor_side
    ~size
    ~price:fill.price
  |> fun t ->
  update_position
    t
    ~participant:fill.resting_participant
    ~symbol:fill.symbol
    ~side:(Side.flip fill.aggressor_side)
    ~size
    ~price:fill.price
;;

let apply_trade_report t ~symbol ~price =
  { t with
    reference_prices = Map.set t.reference_prices ~key:symbol ~data:price
  }
;;

module Position_summary = struct
  type t =
    { symbol : Symbol.t
    ; inventory : int
    ; average_entry_price : Price.t option
    ; reference_price : Price.t option
    ; realized_cents : int
    ; unrealized_cents : int
    }
  [@@deriving sexp_of, fields ~getters]
end

module Summary = struct
  type t =
    { participant : Participant.t
    ; per_symbol : Position_summary.t list
    ; realized_cents : int
    ; unrealized_cents : int
    ; total_cents : int
    }
  [@@deriving sexp_of, fields ~getters]

  let dollars cents = Float.of_int cents /. 100.

  let to_string
    { participant
    ; per_symbol
    ; realized_cents
    ; unrealized_cents
    ; total_cents
    }
    =
    let lines =
      List.map per_symbol ~f:(fun ps ->
        [%string
          "  %{ps.symbol#Symbol}: inv=%{ps.inventory#Int} \
           realized=$%{dollars ps.realized_cents#Float} \
           unrealized=$%{dollars ps.unrealized_cents#Float}"])
    in
    String.concat
      ~sep:"\n"
      (([%string "%{participant#Participant} P&L:"] :: lines)
       @ [ [%string
             "  TOTAL: realized=$%{dollars realized_cents#Float} \
              unrealized=$%{dollars unrealized_cents#Float} \
              total=$%{dollars total_cents#Float}"]
         ])
  ;;
end

let position_summary ~symbol ~reference_price (position : position) =
  let { inventory; cost_basis_cents; realized_cents } = position in
  let average_entry_price =
    match inventory = 0 with
    | true -> None
    | false -> Some (Price.of_int_cents (cost_basis_cents / inventory))
  in
  let unrealized_cents =
    match reference_price with
    | None -> 0
    | Some reference_price ->
      (* [inventory * average_entry = cost_basis], so this is exactly
         [inventory * (reference_price - average_entry_price)]. *)
      (inventory * Price.to_int_cents reference_price) - cost_basis_cents
  in
  { Position_summary.symbol
  ; inventory
  ; average_entry_price
  ; reference_price
  ; realized_cents
  ; unrealized_cents
  }
;;

let summary t ~participant =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, position) ->
      let reference_price = Map.find t.reference_prices symbol in
      position_summary ~symbol ~reference_price position)
  in
  let realized_cents =
    List.sum (module Int) per_symbol ~f:Position_summary.realized_cents
  in
  let unrealized_cents =
    List.sum (module Int) per_symbol ~f:Position_summary.unrealized_cents
  in
  { Summary.participant
  ; per_symbol
  ; realized_cents
  ; unrealized_cents
  ; total_cents = realized_cents + unrealized_cents
  }
;;

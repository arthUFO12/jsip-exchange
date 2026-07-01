(** Per-participant profit-and-loss tracking.

    Given the {!Jsip_types.Fill.t}s a participant trades and the public trade
    prints ({!Jsip_types.Exchange_event.Trade_report}) that set the current
    market price, [Pnl] tracks, for every [(participant, symbol)] pair:

    - the current signed {b inventory} (positive = long, negative = short),
    - a running {b cost basis} for the open position (from which the average
      entry price is derived), and
    - the {b realized} cash booked by closing positions.

    From those it derives, per symbol and in aggregate, {b realized} and
    {b unrealized} P&L:

    - {b Realized} is cash locked in when you close (or reduce) a position:
      the cash you take in from closing minus the cost basis of the shares
      you closed.
    - {b Unrealized} marks the still-open position to a reference price:
      [inventory * (reference_price - average_entry_price)].

    A single {!Jsip_types.Fill.t} involves two participants — an aggressor
    and a resting order — so {!apply_fill} updates both of their positions.
    The reference price used for unrealized P&L is refreshed from public
    trade prints via {!apply_trade_report}.

    Example:
    {[
      let pnl =
        Pnl.empty
        |> fun p ->
        Pnl.apply_fill p fill
        |> fun p -> Pnl.apply_trade_report p ~symbol:aapl ~price:last_print
      in
      let summary = Pnl.summary pnl ~participant:alice in
      print_s [%sexp (summary : Pnl.Summary.t)]
    ]} *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** P&L state in which every participant is flat, no cash is realized, and no
    reference prices are known. *)
val empty : t

(** Fold a fill into the P&L. Both the aggressor and the resting participant
    have their position for [fill.symbol] updated: the aggressor on
    [fill.aggressor_side], the resting participant on the opposite side. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference (mark) price for [symbol] from a public trade
    print. This does not change any inventory or realized cash; it only
    changes how open positions in [symbol] are valued for unrealized P&L.

    (The exchange models a trade print as the
    [Jsip_types.Exchange_event.Trade_report] constructor, which carries a
    [symbol], [price], and [size]. Only the symbol and price matter for a
    mark price, so those are taken directly.) *)
val apply_trade_report : t -> symbol:Symbol.t -> price:Price.t -> t

(** A single [(participant, symbol)] position, valued against the latest
    reference price. *)
module Position_summary : sig
  type t =
    { symbol : Symbol.t
    ; inventory : int (** signed shares; positive long, negative short *)
    ; average_entry_price : Price.t option
    (** average price paid for the open position, or [None] when flat *)
    ; reference_price : Price.t option
    (** latest mark price for the symbol, or [None] if none has been seen *)
    ; realized_cents : int
    ; unrealized_cents : int
    (** [0] when flat or when no reference price is known *)
    }
  [@@deriving sexp_of, fields ~getters]
end

(** A participant's P&L broken down per symbol, with the firm-wide totals. *)
module Summary : sig
  type t =
    { participant : Participant.t
    ; per_symbol : Position_summary.t list
    (** one entry per symbol the participant has ever traded, sorted by
        symbol *)
    ; realized_cents : int (** sum of [realized_cents] across symbols *)
    ; unrealized_cents : int (** sum of [unrealized_cents] across symbols *)
    ; total_cents : int (** [realized_cents + unrealized_cents] *)
    }
  [@@deriving sexp_of, fields ~getters]

  val to_string : t -> string
end

(** The per-symbol breakdown and totals for one participant. A participant
    who has never traded gets an empty [per_symbol] and zeroed totals. *)
val summary : t -> participant:Participant.t -> Summary.t

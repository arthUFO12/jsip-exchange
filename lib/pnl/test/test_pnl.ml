open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness

(* A hand-rolled fill. The order/client ids are irrelevant to P&L — only the
   participants, side, symbol, price, and size matter — so we derive them from
   [fill_id] to keep call sites short. *)
let fill ~fill_id ~aggressor ~aggressor_side ~resting ~price_cents ~size : Fill.t =
  { fill_id
  ; symbol = Harness.aapl
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int fill_id
  ; aggressor_participant = aggressor
  ; aggressor_client_order_id = Client_order_id.of_int fill_id
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int (fill_id + 1000)
  ; resting_participant = resting
  ; resting_client_order_id = Client_order_id.of_int (fill_id + 1000)
  }
;;

module Ps = Pnl.Position_summary

let show pnl ~name ~participant =
  let summary = Pnl.summary pnl ~participant in
  let show_price = function
    | None -> "-"
    | Some p -> Price.to_string_dollar p
  in
  List.iter summary.per_symbol ~f:(fun ps ->
    printf
      "%s %s: inv=%d avg=%s ref=%s realized=%dc unrealized=%dc\n"
      name
      (Symbol.to_string (Ps.symbol ps))
      (Ps.inventory ps)
      (show_price (Ps.average_entry_price ps))
      (show_price (Ps.reference_price ps))
      (Ps.realized_cents ps)
      (Ps.unrealized_cents ps));
  printf
    "%s TOTAL: realized=%dc unrealized=%dc total=%dc\n"
    name
    summary.realized_cents
    summary.unrealized_cents
    summary.total_cents
;;

let%expect_test "open, partially close, then mark to a trade print" =
  (* Alice lifts Bob's offer: she buys 100 AAPL @ $150. *)
  let pnl =
    Pnl.apply_fill
      Pnl.empty
      (fill
         ~fill_id:1
         ~aggressor:Harness.alice
         ~aggressor_side:Buy
         ~resting:Harness.bob
         ~price_cents:15000
         ~size:100)
  in
  (* Alice sells 40 back to Bob @ $155, realizing profit on those 40 shares. *)
  let pnl =
    Pnl.apply_fill
      pnl
      (fill
         ~fill_id:2
         ~aggressor:Harness.alice
         ~aggressor_side:Sell
         ~resting:Harness.bob
         ~price_cents:15500
         ~size:40)
  in
  (* A public trade print at $158 marks everyone's open position. *)
  let pnl = Pnl.apply_trade_report pnl ~symbol:Harness.aapl ~price:(Price.of_int_cents 15800) in
  show pnl ~name:"Alice" ~participant:Harness.alice;
  show pnl ~name:"Bob" ~participant:Harness.bob;
  (* Alice: long 60 @ $150 avg, booked $200 on the 40 she sold ($5 x 40), and
     is up $8 x 60 = $480 unrealized against the $158 mark.

     Bob is the mirror image: short 60 @ $150, down $200 realized and $480
     unrealized. *)
  [%expect
    {|
    Alice AAPL: inv=60 avg=$150.00 ref=$158.00 realized=20000c unrealized=48000c
    Alice TOTAL: realized=20000c unrealized=48000c total=68000c
    Bob AAPL: inv=-60 avg=$150.00 ref=$158.00 realized=-20000c unrealized=-48000c
    Bob TOTAL: realized=-20000c unrealized=-48000c total=-68000c
    |}]
;;

let%expect_test "a flat participant has no positions" =
  show Pnl.empty ~name:"Charlie" ~participant:Harness.charlie;
  [%expect {| Charlie TOTAL: realized=0c unrealized=0c total=0c |}]
;;

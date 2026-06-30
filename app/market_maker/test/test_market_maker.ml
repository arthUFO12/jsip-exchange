(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker
open Jsip_types
open E2e_helpers

let default_symbol_config : Market_maker_data.SymbolConfig.t =
  { symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  ; inventory_skew_cents_per_share = 10
  }
;;

let default_config = 
  ({ participant = Participant.of_string "MarketMaker"
  ; symbol_configs = [ default_symbol_config ]

  } : Market_maker_data.Config.t)

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let mm_data = Market_maker_data.create default_config in
    let%bind () = Market_maker.seed_book mm_data default_symbol_config.symbol default_symbol_config.fair_value_cents (connection mm) in
    [%expect
      {|
      [for MarketMaker] ACCEPTED id=1 AAPL SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED id=2 AAPL SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED id=3 AAPL SELL 100@$150.12 DAY
      [for MarketMaker] ACCEPTED id=4 AAPL BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED id=5 AAPL BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED id=6 AAPL BUY 100@$149.88 DAY
      |}];
    return ())
;;

let%expect_test "run: does things" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as_no_sub ~port Harness.alice in
    let%bind mm = connect_as_no_login ~port Harness.market_maker in
    let%bind () =
      Market_maker.For_testing.run
        default_config
        (connection mm)
    in
    let%bind () =
      rpc_submit alice (Harness.sell ~size:50 ~price_cents:14990 ())
    in
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 10.0) in
    [%expect
      {|
      ACCEPTED id=1 AAPL SELL 100@$150.10 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 9 order_size: 100

      ACCEPTED id=2 AAPL SELL 100@$150.11 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 9 order_size: 100
      client_order_id: 10 order_size: 100

      ACCEPTED id=3 AAPL SELL 100@$150.12 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 11 order_size: 100
      client_order_id: 10 order_size: 100
      client_order_id: 9 order_size: 100

      ACCEPTED id=4 AAPL BUY 100@$149.90 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 11 order_size: 100
      client_order_id: 12 order_size: 100
      client_order_id: 10 order_size: 100
      client_order_id: 9 order_size: 100

      ACCEPTED id=5 AAPL BUY 100@$149.89 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 12 order_size: 100
      client_order_id: 13 order_size: 100
      client_order_id: 11 order_size: 100
      client_order_id: 10 order_size: 100
      client_order_id: 9 order_size: 100

      ACCEPTED id=6 AAPL BUY 100@$149.88 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 12 order_size: 100
      client_order_id: 13 order_size: 100
      client_order_id: 14 order_size: 100
      client_order_id: 11 order_size: 100
      client_order_id: 10 order_size: 100
      client_order_id: 9 order_size: 100

      FILL fill_id=1 AAPL $149.90 x50 aggressor=7(Alice) aggressor_coid=(15) SELL resting=4(MarketMaker) resting_coid=(12)

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50
      client_order_id: 13 order_size: 100
      client_order_id: 14 order_size: 100
      client_order_id: 11 order_size: 100
      client_order_id: 10 order_size: 100
      client_order_id: 9 order_size: 100

      CANCELLED id=1 AAPL client_order_id=9 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50
      client_order_id: 13 order_size: 100
      client_order_id: 14 order_size: 100
      client_order_id: 11 order_size: 100
      client_order_id: 10 order_size: 100

      CANCELLED id=2 AAPL client_order_id=10 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50
      client_order_id: 13 order_size: 100
      client_order_id: 14 order_size: 100
      client_order_id: 11 order_size: 100

      CANCELLED id=3 AAPL client_order_id=11 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50
      client_order_id: 13 order_size: 100
      client_order_id: 14 order_size: 100

      CANCELLED id=6 AAPL client_order_id=14 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50
      client_order_id: 13 order_size: 100

      CANCELLED id=5 AAPL client_order_id=13 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 12 order_size: 50

      CANCELLED id=4 AAPL client_order_id=12 remaining=50 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL

      ACCEPTED id=8 AAPL SELL 100@$145.10 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100

      ACCEPTED id=9 AAPL SELL 100@$145.11 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100
      client_order_id: 17 order_size: 100

      ACCEPTED id=10 AAPL SELL 100@$145.12 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100
      client_order_id: 17 order_size: 100
      client_order_id: 18 order_size: 100

      ACCEPTED id=11 AAPL BUY 100@$144.90 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100
      client_order_id: 17 order_size: 100
      client_order_id: 18 order_size: 100
      client_order_id: 19 order_size: 100

      ACCEPTED id=12 AAPL BUY 100@$144.89 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100
      client_order_id: 20 order_size: 100
      client_order_id: 17 order_size: 100
      client_order_id: 18 order_size: 100
      client_order_id: 19 order_size: 100

      ACCEPTED id=13 AAPL BUY 100@$144.88 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 16 order_size: 100
      client_order_id: 20 order_size: 100
      client_order_id: 21 order_size: 100
      client_order_id: 17 order_size: 100
      client_order_id: 18 order_size: 100
      client_order_id: 19 order_size: 100
      |}];
    return ())
;;

let%expect_test "run: reposts after receiving a fill event" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as_no_sub ~port Harness.alice in
    let%bind mm = connect_as_no_login ~port Harness.market_maker in
    let%bind () =
      Market_maker.For_testing.run
        default_config
        (connection mm)
    in
    let%bind () =
      rpc_submit alice (Harness.sell ~size:50 ~price_cents:14990 ())
    in
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 10.0) in
    
    [%expect {|
      ACCEPTED id=1 AAPL SELL 100@$150.10 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100

      ACCEPTED id=2 AAPL SELL 100@$150.11 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 23 order_size: 100

      ACCEPTED id=3 AAPL SELL 100@$150.12 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 23 order_size: 100

      ACCEPTED id=4 AAPL BUY 100@$149.90 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 25 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 23 order_size: 100

      ACCEPTED id=5 AAPL BUY 100@$149.89 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 100
      client_order_id: 23 order_size: 100
      client_order_id: 26 order_size: 100

      ACCEPTED id=6 AAPL BUY 100@$149.88 DAY

      symbol: AAPL inventory_size: 0
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 100
      client_order_id: 27 order_size: 100
      client_order_id: 23 order_size: 100
      client_order_id: 26 order_size: 100

      FILL fill_id=1 AAPL $149.90 x50 aggressor=7(Alice) aggressor_coid=(28) SELL resting=4(MarketMaker) resting_coid=(25)

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 50
      client_order_id: 27 order_size: 100
      client_order_id: 23 order_size: 100
      client_order_id: 26 order_size: 100

      CANCELLED id=5 AAPL client_order_id=26 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 50
      client_order_id: 27 order_size: 100
      client_order_id: 23 order_size: 100

      CANCELLED id=2 AAPL client_order_id=23 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 50
      client_order_id: 27 order_size: 100

      CANCELLED id=6 AAPL client_order_id=27 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100
      client_order_id: 25 order_size: 50

      CANCELLED id=4 AAPL client_order_id=25 remaining=50 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100
      client_order_id: 24 order_size: 100

      CANCELLED id=3 AAPL client_order_id=24 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 22 order_size: 100

      CANCELLED id=1 AAPL client_order_id=22 remaining=100 reason=PARTICIPANT_REQUESTED

      symbol: AAPL inventory_size: 50
      symbol: AAPL

      ACCEPTED id=8 AAPL SELL 100@$145.10 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 29 order_size: 100

      ACCEPTED id=9 AAPL SELL 100@$145.11 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 30 order_size: 100
      client_order_id: 29 order_size: 100

      ACCEPTED id=10 AAPL SELL 100@$145.12 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 30 order_size: 100
      client_order_id: 29 order_size: 100
      client_order_id: 31 order_size: 100

      ACCEPTED id=11 AAPL BUY 100@$144.90 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 32 order_size: 100
      client_order_id: 30 order_size: 100
      client_order_id: 29 order_size: 100
      client_order_id: 31 order_size: 100

      ACCEPTED id=12 AAPL BUY 100@$144.89 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 32 order_size: 100
      client_order_id: 30 order_size: 100
      client_order_id: 29 order_size: 100
      client_order_id: 33 order_size: 100
      client_order_id: 31 order_size: 100

      ACCEPTED id=13 AAPL BUY 100@$144.88 DAY

      symbol: AAPL inventory_size: 50
      symbol: AAPL
      client_order_id: 32 order_size: 100
      client_order_id: 34 order_size: 100
      client_order_id: 30 order_size: 100
      client_order_id: 29 order_size: 100
      client_order_id: 33 order_size: 100
      client_order_id: 31 order_size: 100
      |}];
    return ())
;;

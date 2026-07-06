open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

(* --- Event formatting --- *)

let%expect_test "format_event: all event types" =
  let events =
    [ Exchange_event.Order_accept
        { order_id = Order_id.of_string "1"
        ; participant = Participant.of_string "Alice"
        ; request =
            { symbol = Symbol.of_string "AAPL"
            ; participant = Participant.of_string "Alice"
            ; side = Buy
            ; price = Price.of_int_cents 15000
            ; size = Size.of_int 100
            ; time_in_force = Day
            ; client_order_id = Client_order_id.of_string "1"
            }
        }
    ; Fill
        { fill_id = 1
        ; symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        ; aggressor_order_id = Order_id.of_string "2"
        ; aggressor_participant = Participant.of_string "Alice"
        ; aggressor_client_order_id = Client_order_id.of_string "2"
        ; aggressor_side = Buy
        ; resting_order_id = Order_id.of_string "1"
        ; resting_participant = Participant.of_string "Bob"
        ; resting_client_order_id = Client_order_id.of_string "1"
        }
    ; Order_cancel
        { order_id = Order_id.of_string "3"
        ; participant = Participant.of_string "Charlie"
        ; symbol = Symbol.of_string "TSLA"
        ; remaining_size = Size.of_int 50
        ; reason = Ioc_remainder
        ; client_order_id = Client_order_id.of_string "3"
        }
    ; Order_reject
        { participant = Participant.of_string "Alice"
        ; request =
            { symbol = Symbol.of_string "GOOG"
            ; participant = Participant.of_string "Alice"
            ; side = Sell
            ; price = Price.of_int_cents 28000
            ; size = Size.of_int 10
            ; time_in_force = Day
            ; client_order_id = Client_order_id.of_string "4"
            }
        ; reason = "unknown symbol"
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"
        ; bbo =
            { bid =
                Some
                  { price = Price.of_int_cents 14990
                  ; size = Size.of_int 200
                  }
            ; ask =
                Some
                  { price = Price.of_int_cents 15010
                  ; size = Size.of_int 100
                  }
            }
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"; bbo = Bbo.empty }
    ; Trade_report
        { symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        }
    ]
  in
  List.iter events ~f:(fun e -> print_endline (Protocol.format_event e));
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) aggressor_coid=(2) BUY resting=1(Bob) resting_coid=(1)
    CANCELLED id=3 TSLA client_order_id=3 remaining=50 reason=IOC_REMAINDER
    REJECTED GOOG SELL 10@$280.00 reason=unknown symbol
    BBO AAPL bid=$149.90 x200 ask=$150.10 x100
    BBO AAPL bid=- ask=-
    TRADE AAPL $150.00 x100
    |}]
;;

(* --- Round-trip: parse then format --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* Parse a buy command from text and submit it *)
  let request =
    Exchange_command.parse ~participant:Harness.alice "BUY 1 AAPL 100 150.00"
    |> ok_exn
  in
  match (request : Exchange_command.t) with
  | Book _ -> print_endline "Error"
  | Subscribe _ -> print_endline "Error"
  | Cancel _ -> print_endline "Error"
  | Submit submit ->
    let _order, events = Matching_engine.submit (Harness.engine t) submit in
    print_endline (Protocol.format_events events);
    [%expect
      {|
      ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
      BBO AAPL bid=- ask=$150.00 x100
      ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
      FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) aggressor_coid=(1) BUY resting=1(Bob) resting_coid=(3)
      TRADE AAPL $150.00 x100
      BBO AAPL bid=- ask=-
      |}]
;;

open! Core
open! Jsip_types
open Jsip_gateway

let print_parse line =
  match
    Or_error.join (Or_error.try_with (fun () -> Exchange_command.parse line))
  with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok comm -> print_endline [%string "%{comm#Exchange_command}"]
;;

(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY 1 AAPL 100 150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY (id: 1) |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL 1 TSLA 50 200.00";
  [%expect {| SELL TSLA 50@$200.00 DAY (id: 1) |}]
;;

let%expect_test "parse: basic cancel" =
  print_parse "CANCEL 1";
  [%expect {| 1 |}]
;;

let%expect_test "parse: basic book" =
  print_parse "BOOK AAPL";
  [%expect {| AAPL |}]
;;

let%expect_test "parse: basic subscribe" =
  print_parse "SUBSCRIBE AAPL";
  [%expect {| AAPL |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy 1 AAPL 100 150.00";
  print_parse "Buy 1 AAPL 100 150.00";
  [%expect
    {|
    BUY AAPL 100@$150.00 DAY (id: 1)
    BUY AAPL 100@$150.00 DAY (id: 1)
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY 1 AAPL 100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC (id: 1) |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL 1 AAPL 200 151.00 DAY";
  [%expect {| SELL AAPL 200@$151.00 DAY (id: 1) |}]
;;

let%expect_test "parse: with TIF" =
  print_parse "SELL 1 GOOG 75 2800.50 IOC";
  [%expect {| SELL GOOG 75@$2800.50 IOC (id: 1) |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY 1 aapl 100 150.00";
  [%expect {| BUY AAPL 100@$150.00 DAY (id: 1) |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "  BUY 1    AAPL   100   150.00  ";
  [%expect {| BUY AAPL 100@$150.00 DAY (id: 1) |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY 1 AAPL 100 $150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY (id: 1) |}]
;;

(* --- Parse errors --- *)

let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse "   ";
  [%expect {|
    ERROR: empty command
    ERROR: empty command
    |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse "HOLD 1 AAPL 100 150.00";
  [%expect
    {| ERROR: ("Exchange_command.Verb.of_string: invalid string" (value HOLD)) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY 1 AAPL";
  print_parse "BUY";
  [%expect
    {|
    ERROR: expected: BUY|SELL <coid> <symbol> <size> <price> [DAY|IOC]
    ERROR: expected: BUY|SELL <coid> <symbol> <size> <price> [DAY|IOC]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY 1 AAPL abc 150.00";
  print_parse "BUY 1 AAPL 0 150.00";
  print_parse "BUY 1 AAPL -5 150.00";
  [%expect
    {|
    ERROR: (Failure "Int.of_string: \"abc\"")
    BUY AAPL 0@$150.00 DAY (id: 1)
    BUY AAPL -5@$150.00 DAY (id: 1)
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY 1 AAPL 100 xyz";
  [%expect {|
    ERROR: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY 1 AAPL 100 150.00 QQQ";
  [%expect
    {| ERROR: ("Time_in_force.of_string: invalid string" (value QQQ)) |}]
;;

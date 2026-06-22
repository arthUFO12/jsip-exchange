open! Core
open Jsip_types
open Jsip_gateway


let print_parse line =
  match Or_error.join (Or_error.try_with (fun () -> Exchange_command.parse line)) with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok comm -> print_endline [%string "%{comm#Exchange_command}"]
;;

let submit_or_error comm =
  match (comm : Exchange_command.t Or_error.t) with
  | Error e -> Or_error.error_string (Error.to_string_hum e)
  | Ok valid_comm ->
    (match valid_comm with
     | Submit s -> Or_error.return s
     | _ -> Or_error.error_string "Wrong type")
;;



(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY AAPL 100 150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00";
  [%expect {| SELL TSLA 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100 150.00";
  print_parse "Buy AAPL 100 150.00";
  [%expect
    {|
    BUY AAPL 100@$150.00 DAY as anonymous
    BUY AAPL 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL 100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL AAPL 200 151.00 DAY";
  [%expect {| SELL AAPL 200@$151.00 DAY as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100 150.00 as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

let%expect_test "parse: with TIF and participant" =
  print_parse "SELL GOOG 75 2800.50 IOC as Bob";
  [%expect {| SELL GOOG 75@$2800.50 IOC as Bob |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY aapl 100 150.00";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "  BUY   AAPL   100   150.00  ";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY AAPL 100 $150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
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
  print_parse "HOLD AAPL 100 150.00";
  [%expect {| ERROR: ("Exchange_command.Verb.of_string: invalid string" (value HOLD)) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY AAPL";
  print_parse "BUY";
  [%expect
    {|
    ERROR: expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc 150.00";
  print_parse "BUY AAPL 0 150.00";
  print_parse "BUY AAPL -5 150.00";
  [%expect
    {|
    ERROR: (Failure "Int.of_string: \"abc\"")
    BUY AAPL 0@$150.00 DAY as anonymous
    BUY AAPL -5@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY AAPL 100 xyz";
  [%expect
    {|
    ERROR: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY AAPL 100 150.00 QQQ";
  [%expect {| ERROR: ("Time_in_force.of_string: invalid string" (value QQQ)) |}]
;;

(* --- parse_command_with_default_participant --- *)

let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    Exchange_command.parse "BUY AAPL 100 150.00" ~default_participant:default
    |> submit_or_error
    |> ok_exn
  in
  print_endline [%string "participant=%{req.participant#Participant}"];
  [%expect {| participant=DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit 'as'" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    Exchange_command.parse
      "BUY AAPL 100 150.00 as Alice"
      ~default_participant:default
    |> submit_or_error
    |> ok_exn
  in
  print_endline [%string "participant=%{req.participant#Participant}"];
  [%expect {| participant=Alice |}]
;;

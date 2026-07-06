open! Core
open Jsip_types
open Expect_test_helpers_core

let%expect_test "of_string: empty string raises" =
  require_does_raise (fun () -> Symbol.of_string "");
  [%expect
    {| "Symbol.of_string: symbol must be a non-empty alphanumeric string" |}]
;;

let%expect_test "of_string: non-alphanumeric string raises" =
  require_does_raise (fun () -> Symbol.of_string "AAPL~");
  [%expect
    {| "Symbol.of_string: symbol must be a non-empty alphanumeric string" |}]
;;

let%expect_test "of_string: capitalized alphanumeric string passes" =
  [%test_result: string]
    (let aapl = "AAPL" in
     [%string "%{Symbol.of_string aapl#Symbol}"])
    ~expect:"AAPL"
;;

let%expect_test "of_string: uncapitalized alphanumeric string passes and is \
                 capitalized"
  =
  [%test_result: string]
    (let aapl = "aapl" in
     [%string "%{Symbol.of_string aapl#Symbol}"])
    ~expect:"AAPL"
;;

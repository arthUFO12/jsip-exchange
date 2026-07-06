open! Core
open! Jsip_types

(* Verb.t type: For deducing the type of an exchange command *)
module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
    | Cancel
  [@@deriving
    sexp, string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]
end

(* Exchange_command.t: variant type to represent all types of exchange
   commands *)
type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
  | Cancel of Client_order_id.t

let handle_book_or_subscribe
  (book_or_subscribe : Verb.t)
  (rest_of_line : string list)
  : t Or_error.t
  =
  match book_or_subscribe, rest_of_line with
  | Book, ticker :: _ -> Or_error.return (Book (Symbol.of_string ticker))
  | Subscribe, ticker :: _ ->
    Or_error.return (Subscribe (Symbol.of_string ticker))
  | _, _ :: _ ->
    Or_error.error_string
      "Used handle book or subscribe for incorrect command"
  | _, [] -> Or_error.error_string "No ticker provided"
;;

let handle_cancel (rest_of_line : string list) : t Or_error.t =
  match rest_of_line with
  | coid_str :: _ -> Cancel (Client_order_id.of_string coid_str) |> Ok
  | [] -> Or_error.error_string "no client order id given"
;;

let handle_buy_or_sell (buy_or_sell : Verb.t) (rest_of_line : string list)
  : t Or_error.t
  =
  match buy_or_sell with
  | Subscribe -> Or_error.error_string "Wrong func"
  | Book -> Or_error.error_string "Wrong func"
  | _ ->
    (match rest_of_line with
     (* for SELL and BUY, command should have
        [ verb symbol size price time_in_force optional<participant> ] *)
     | client_order_id_str
       :: symbol_str
       :: size_str
       :: price_str
       :: maybe_tif ->
       let client_order_id = Client_order_id.of_string client_order_id_str in
       let size = Size.of_string size_str in
       let symbol = Symbol.of_string symbol_str in
       let price = Price.of_string price_str in
       let time_in_force =
         match maybe_tif with
         | [] -> Time_in_force.Day
         | tif_str :: _ -> Time_in_force.of_string tif_str
       in
       Ok
         (Submit
            { side =
                (match buy_or_sell with
                 | Buy -> Side.Buy
                 | Sell -> Side.Sell
                 | _ -> Side.Buy)
            ; size
            ; price
            ; time_in_force
            ; symbol
            ; client_order_id
            })
     | _ ->
       Or_error.error_string
         "expected: BUY|SELL <coid> <symbol> <size> <price> [DAY|IOC]")
;;

(* parse_exn: Sole function for parsing commands sent to the exchange. Can
   throw exceptions Input: Command string sent to the exchange with an
   optional default_participant argument Output: Exchange_command.t
   describing the string command sent to the exchange *)

let parse command_string : t Or_error.t =
  match
    String.split command_string ~on:' '
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun str -> not (String.is_empty str))
  with
  | [] -> Or_error.error_string "empty command"
  | first_word :: rest ->
    let verb = Verb.of_string first_word in
    (* Convert first word of string command to Verb.t so we know what type of
       command it is *)
    (match verb with
     | (Book | Subscribe) as book_or_sub ->
       handle_book_or_subscribe book_or_sub rest
       (* in the BOOK case, we just return a Book.t variant of the exchange
          command with the ticker *)
     | (Buy | Sell) as buy_or_sell -> handle_buy_or_sell buy_or_sell rest
     | Cancel -> handle_cancel rest)
;;

let to_string comm =
  match comm with
  | Book book -> Symbol.to_string book
  | Subscribe subscribe -> Symbol.to_string subscribe
  | Submit submit -> Order.Request.to_string submit
  | Cancel cancel -> Client_order_id.to_string cancel
;;

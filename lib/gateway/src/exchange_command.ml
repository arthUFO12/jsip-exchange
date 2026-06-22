open! Core
open! Jsip_types

(* Verb.t type: For deducing the type of an exchange command *)
module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving
    sexp, string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]
end

(* Exchange_command.t: variant type to represent all types of exchange
   commands *)
type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

let handle_book_or_subscribe
  (book_or_subscribe : Verb.t)
  (rest_of_line : string list)
  : t Or_error.t
  =
  match book_or_subscribe, rest_of_line with
  | Book, ticker :: _ -> Or_error.return (Book (Symbol.of_string ticker))
  | Subscribe, ticker :: _ ->
    Or_error.return (Subscribe (Symbol.of_string ticker))
  | _, _ :: __ ->
    Or_error.error_string
      "Used handle book or subscribe for incorrect command"
  | _, [] -> Or_error.error_string "No ticker provided"
;;

let handle_buy_or_sell
  (buy_or_sell : Verb.t)
  (rest_of_line : string list)
  ~(default_participant : Participant.t option)
  : t Or_error.t
  =
  match buy_or_sell with
  | Subscribe -> Or_error.error_string "Wrong func"
  | Book -> Or_error.error_string "Wrong func"
  | _ ->
    (match rest_of_line with
     (* for SELL and BUY, command should have
        [ verb symbol size price time_in_force optional<participant> ] *)
     | symbol :: size :: price :: time_in_force :: rest' ->
       let tif_and_participant_or_error =
         (* match time_in_force and participant name depending on if the word
            AS is present *)
         match String.uppercase time_in_force, rest' with
         | "AS", participant_name :: _ ->
           Or_error.return ("DAY", participant_name)
         | str, "AS" :: participant_name :: __ ->
           Or_error.return (str, participant_name)
         | str, [] -> Or_error.return (str, "anonymous")
         | _, _ :: _ -> Or_error.error_string "Incorrectly formatted"
       in
       (match tif_and_participant_or_error with
        | Error err -> Or_error.error_string (Error.to_string_hum err)
        | Ok tuple ->
          let tif_str, possible_participant = tuple in
          Or_error.return
            (Submit
               { side =
                   (match buy_or_sell with
                    | Buy -> Side.Buy
                    | Sell -> Side.Sell
                    | _ -> Side.Buy)
               ; size = Size.of_string size
               ; price = Price.of_string price
               ; time_in_force = Time_in_force.of_string tif_str
               ; participant =
                   (match possible_participant, default_participant with
                    | new_participant, None ->
                      Participant.of_string new_participant
                    | _, Some valid_participant -> valid_participant)
               ; symbol = Symbol.of_string symbol
               }))
     | _ -> Or_error.error_string "Incomplete buy request")
;;

(* parse_exn: Sole function for parsing commands sent to the exchange. Can
   throw exceptions Input: Command string sent to the exchange with an
   optional default_participant argument Output: Exchange_command.t
   describing the string command sent to the exchange *)

let parse ?default_participant command_string : t Or_error.t =
  match String.split command_string ~on:' ' with
  | [] -> Or_error.error_string "Error: Received empty string command"
  | first_word :: rest ->
    let verb = Verb.of_string first_word in
    (* Convert first word of string command to Verb.t so we know what type of
       command it is *)
    (match verb with
     | Book ->
       handle_book_or_subscribe Book rest
       (* in the BOOK case, we just return a Book.t variant of the exchange
          command with the ticker *)
     | Subscribe -> handle_book_or_subscribe Subscribe rest
     | Buy -> handle_buy_or_sell Buy rest ~default_participant
     | Sell -> handle_buy_or_sell Sell rest ~default_participant)
;;

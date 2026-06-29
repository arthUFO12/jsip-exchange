open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_client_order_id : Client_order_id.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
  ; resting_participant : Participant.t
  ; resting_client_order_id : Client_order_id.t
  }
[@@deriving sexp, bin_io]

let to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_order_id
   ; aggressor_participant
   ; aggressor_client_order_id
   ; aggressor_side
   ; resting_order_id
   ; resting_participant
   ; resting_client_order_id
   } :
    t)
  =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s(%s) aggressor_coid=(%s) %s resting=%s(%s) resting_coid=(%s)"
    fill_id
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Client_order_id.to_string aggressor_client_order_id)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
    (Client_order_id.to_string resting_client_order_id)
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size

let to_participant_view t = 
  [%string "You bought %{t.size#Size} %{t.symbol#Symbol} at %{t.price#Price}"]
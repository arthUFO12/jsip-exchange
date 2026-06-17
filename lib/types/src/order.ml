open! Core

module Request = struct
  type t =
    { symbol : Symbol.t
    ; participant : Participant.t
    ; side : Side.t
    ; price : Price.t
    ; size : Size.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp, bin_io]

  let to_string { symbol; participant; side; price; size; time_in_force } =
    let price = Price.to_string_dollar price in
    let size = Size.to_int size in
    [%string
      "%{side#Side} %{symbol#Symbol} %{size#Int}@%{price} \
       %{time_in_force#Time_in_force} as %{participant#Participant}"]
  ;;
end

type t =
  { order_id : Order_id.t
  ; symbol : Symbol.t
  ; participant : Participant.t
  ; side : Side.t
  ; price : Price.t
  ; size : Size.t
  ; mutable remaining_size : Size.t
  ; time_in_force : Time_in_force.t
  }
[@@deriving sexp_of, equal, compare]

let to_string
  ({ order_id
   ; symbol = _
   ; participant
   ; side = _
   ; price
   ; size = _
   ; remaining_size
   ; time_in_force = _
   } :
    t)
  =
  let price = Price.to_string_dollar price in
  let size = Size.to_int remaining_size in
  [%string
    "%{price} x%{size#Int} (id=%{order_id#Order_id}, \
     %{participant#Participant})"]
;;

let create (req : Request.t) ~order_id =
  if Size.( <= ) req.size Size.zero
  then
    raise_s
      [%message "Order.create: size must be positive" (req.size : Size.t)];
  { order_id
  ; symbol = req.symbol
  ; participant = req.participant
  ; side = req.side
  ; price = req.price
  ; size = req.size
  ; remaining_size = req.size
  ; time_in_force = req.time_in_force
  }
;;

let order_id t = t.order_id
let symbol t = t.symbol
let participant t = t.participant
let side t = t.side
let price t = t.price
let size t = t.size
let remaining_size t = t.remaining_size
let time_in_force t = t.time_in_force

let better_price_time side ~ord1 ~ord2 = 
  let ord1_price = price ord1 in
  let ord2_price = price ord2 in
  let ord1_id = order_id ord1 in
  let ord2_id = order_id ord2 in
    if (Price.equal ord1_price ord2_price) then Order_id.(<) ord1_id ord2_id
    else Price.is_more_aggressive side ~price:ord1_price ~than:ord2_price


let price_time_cmp side ord1 ord2 =
  let ord1_price = price ord1 in
  let ord2_price = price ord2 in
  let ord1_id = order_id ord1 in
  let ord2_id = order_id ord2 in
    if (Price.equal ord1_price ord2_price) then Order_id.compare ord1_id ord2_id
    else (if Price.is_more_aggressive side ~price:ord1_price ~than:ord2_price then 1 else -1)


let fill t ~by =
  if Size.( <= ) by Size.zero
  then
    raise_s [%message "Order.fill: fill size must be positive" (by : Size.t)];
  if Size.( > ) by t.remaining_size
  then
    raise_s
      [%message
        "Order.fill: fill size exceeds remaining"
          (by : Size.t)
          (t.remaining_size : Size.t)
          (t.order_id : Order_id.t)];
  t.remaining_size <- Size.( - ) t.remaining_size by
;;

let is_fully_filled t = Size.equal t.remaining_size Size.zero

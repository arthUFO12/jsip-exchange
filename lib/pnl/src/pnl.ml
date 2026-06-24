open! Core
open Jsip_types

module TickerData = struct
  type t = 
  { mutable amount: Size.t
  ; mutable total_price: Price.t 
  ; mutable trading_price: Price.t
  }

  let create () =
    { amount = Size.zero; total_price = Price.zero; trading_price = Price.zero}
  
  let average_entry_price t =
    (Price.to_float t.total_price) /. (Size.to_int t.amount |> Int.to_float)

  let update ?amount ?total_price ?trading_price t =
    let amt = match amount with None -> t.amount | Some num -> num in
    let tot_price = match total_price with None -> t.total_price | Some price -> price in
    let trad_price = match trading_price with None -> t.trading_price | Some price -> price in
    t.amount <- amt;
    t.total_price <- tot_price;
    t.trading_price <- trad_price
  ;;

  let buy_ticker t ~size ~price =
    let total_order_price = (Int.to_float(Size.to_int size)) *. (Price.to_float price) |> Price.of_float_exn in
    update t ~amount:(Size.(+) t.amount size) ~total_price:(Price.(+) t.total_price total_order_price)

  let sell_ticker t ~size ~price =
    let total_order_price = (Int.to_float(Size.to_int size)) *. (Price.to_float price) |> Price.of_float_exn in
    update t ~amount:(Size.(-) t.amount size) ~total_price:(Price.(-) t.total_price total_order_price)


  let amount t = t.amount

  let _total_price t = t.total_price

  let _trading_price t = t.trading_price
end


module ParticipantPnl = struct
  type t =
  { participant: Participant.t
  ; ticker_data: (Symbol.t, TickerData.t) Hashtbl_intf.Hashtbl.t
  ; mutable unrealized_pnl: Price.t
  ; mutable realized_pnl: Price.t
  }

  let _create participant =
    { participant
    ; ticker_data = Hashtbl.create (module Symbol)
    ; unrealized_pnl = Price.of_int_cents 0
    ; realized_pnl = Price.of_int_cents 0
    }
  
  let _calculate_unrealized_pnl t =
    let fold_func ~key:_ ~data:t_data sum =
      match TickerData.amount t_data with 
        | amt when Size.to_int amt >= 0 -> (Price.to_float t_data.trading_price) -. (TickerData.average_entry_price t_data) +. sum
        | _ -> (TickerData.average_entry_price t_data) -. (Price.to_float t_data.trading_price) +. sum
    in
    Hashtbl.fold t.ticker_data ~init:0.0 ~f:fold_func
  
  let exit_position t t_data ~side ~price ~size:(size: Size.t) =
    let amount = TickerData.amount t_data in
    let price_float = Price.to_float price in
    if (Side.equal side Buy  && Size.(>) amount Size.zero)
      || (Side.equal side Sell && Size.(<) amount Size.zero) then 
    let abs_amount = Size.to_int amount |> Int.abs in
    let exit_amount = min abs_amount (Size.to_int size) in
    let avg_entry_price = TickerData.average_entry_price t_data in
    let tot_entry_price = (Int.to_float exit_amount) *. avg_entry_price in
    let tot_exit_price = (Int.to_float exit_amount) *. price_float in
    let transaction_pnl = match (side: Side.t) with Buy -> (tot_exit_price -. tot_entry_price) | Sell -> (tot_entry_price -. tot_exit_price) in
    t.realized_pnl <- (Price.to_float t.realized_pnl) +. transaction_pnl |> Price.of_float_exn

  let _trade_ticker t ~side ~symbol ~price ~size =
    let t_data = Hashtbl.find_or_add t.ticker_data symbol ~default:TickerData.create in
    exit_position t t_data ~side ~price ~size;
    match (side: Side.t) with 
      | Buy -> TickerData.buy_ticker t_data ~price ~size;
      | Sell -> TickerData.sell_ticker t_data ~price ~size;
end
  

type t =
{ participant_data: (Participant.t, ParticipantPnl.t) Hashtbl_intf.Hashtbl.t
}



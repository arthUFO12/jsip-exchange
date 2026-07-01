(** A simple market-making bot.

    A market maker provides liquidity by continuously quoting both a bid
    (buy) and an ask (sell) price. They profit from the spread between the
    two prices, but take risk if the market moves against their inventory.

    This bot places a fixed set of resting orders on both sides of the book
    around a configured "fair value" price. It does not dynamically adjust
    its quotes in response to fills -- that is left as an extension. *)

open! Core
open! Async
open Jsip_types

val update_market_maker_data
  :  Market_maker_data.t
  -> Exchange_event.t
  -> unit

val repost_after_fill
  :  Market_maker_data.t
  -> Exchange_event.t
  -> (Order.Request.t -> unit Deferred.t)
  -> (Client_order_id.t -> unit Deferred.t)
  -> unit

(** Submit the market maker's initial set of resting orders over the given
    open [Rpc.Connection.t]. The connection must already be logged in as
    [config.participant]. [submit_order_rpc] is one-way, so this function
    only returns success/failure of the submission attempt; the actual
    matching-engine response (acceptance, fills, rejection) arrives on the
    participant's session feed. *)
val seed_book
  :  Market_maker_data.t
  -> Symbol.t
  -> int
  -> (Order.Request.t -> unit Deferred.t)
  -> unit Deferred.t

val run : Market_maker_data.Config.t -> Rpc.Connection.t -> unit Deferred.t

module For_testing : sig
  val run : Market_maker_data.Config.t -> Rpc.Connection.t -> unit Deferred.t
end

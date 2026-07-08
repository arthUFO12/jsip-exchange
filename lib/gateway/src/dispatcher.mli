(** Central event-routing component for the gateway.

    Owns subscription registries:

    - **Market-data subscribers**, keyed by [Symbol.t]. Each subscriber gets
      a pipe of [Best_bid_offer_update] and [Trade_report] events for the
      symbol they asked about. This is the public market-data feed.

    - **Audit subscribers**, an unfiltered firehose of every event the
      matching engine produces. Intended for the exchange operator's monitor;
      not appropriate to expose to ordinary clients.

    [dispatch] is the single place that decides "for each event, who gets
    it". *)

open! Core
open! Async
open Jsip_types

type t

(** Create a dispatcher.

    The three optional [max_*_pipe_length] arguments bound how many events
    may buffer for a slow subscriber before the dispatcher starts dropping
    (market data / audit) or evicting the session (per-session). Each
    defaults to a shared built-in value; tune them independently to
    reproduce slow-consumer backpressure. *)
val create
  :  ?max_market_data_pipe_length:int
  -> ?max_audit_pipe_length:int
  -> ?max_session_pipe_length:int
  -> unit
  -> t

(** Subscribe to public market data for one or more [symbols]. The same pipe
    receives events for every requested symbol; the dispatcher avoids
    duplicates so a subscriber listed against multiple symbols only sees each
    event once. The pipe is removed from the dispatcher when its reader is
    closed. *)
val subscribe_market_data
  :  t
  -> Symbol.t list
  -> Exchange_event.t Pipe.Reader.t

(** Subscribe to the full unfiltered event firehose. Intended for the monitor
    / admin tools. *)
val subscribe_audit : t -> Exchange_event.t Pipe.Reader.t

(** Route each event to every interested subscriber:

    - Every event is pushed to every audit subscriber.
    - [Best_bid_offer_update] and [Trade_report] are pushed to the
      market-data subscribers that asked for the event's symbol.
    - [Order_accept], [Order_cancel], and [Order_reject] are pushed to the
      session of the order's owning participant (if logged in).
    - [Fill] is pushed to both the aggressor's and the resting party's
      session (if either is logged in).

    Each session lookup is O(1) and independent of subscriber count. *)
val dispatch : t -> Exchange_event.t list -> unit

val clean_up_session : t -> Session.t -> unit Deferred.t
val set_up_session : t -> Participant.t -> unit Deferred.t
val get_session_exn : t -> Participant.t -> Session.t
val get_session : t -> Participant.t -> Session.t option

val register_coid_to_participant
  :  t
  -> participant:Participant.t
  -> client_order_id:Client_order_id.t
  -> bool

val register_order_to_coid_participant_pair
  :  t
  -> participant:Participant.t
  -> client_order_id:Client_order_id.t
  -> order:Order.t
  -> unit

val get_order
  :  t
  -> participant:Participant.t
  -> client_order_id:Client_order_id.t
  -> Order.t option

module For_testing : sig
  val audit_subscriber_count : t -> int
end

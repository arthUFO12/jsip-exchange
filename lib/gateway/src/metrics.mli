(** Aggregates the live metrics behind {!Rpc_protocol.monitor_feed_rpc} and
    assembles them into a {!Monitor_snapshot.t}.

    (Named [Metrics] rather than [Monitor] to avoid clashing with
    {!Async.Monitor}, which every [open! Async] brings into scope.)

    Like {!Latency_tracker}, this is a pure state machine: the async server
    layer feeds it observations ([record_submit]/[record_cancel], called from
    the matching loop) and, once per second, asks it to [build_snapshot] from
    inputs it supplies — the current time, the process memory reading, and
    the order books. Nothing here reads the wall clock, samples the GC, or
    touches Async, so it is unit testable in isolation.

    It owns two kinds of state:
    - a {!Latency_tracker} for submit/cancel latency, and
    - a trailing one-second window of submission timestamps per participant,
      from which order rate is derived.

    Resting-order counts and book depth are {e not} tracked here — they are
    read straight from the books at snapshot time, since the books already
    are the source of truth for resting interest. *)

open! Core
open Jsip_types
open Jsip_order_book

type t

val create : unit -> t

(** Record a completed submit-order request for [participant], measured at
    [~now] with the given [~latency]. Feeds both the latency tracker and the
    participant's order-rate window. *)
val record_submit
  :  t
  -> now:Time_ns.t
  -> latency:Time_ns.Span.t
  -> participant:Participant.t
  -> unit

(** Record a completed cancel-order request. Cancels are not attributed to a
    participant in the rate metric, so no participant is required. *)
val record_cancel : t -> now:Time_ns.t -> latency:Time_ns.Span.t -> unit

(** Assemble a snapshot as of [~now].

    [~memory] is the process memory and GC reading (from [Gc.stat] in the
    async layer). [~books] is every traded symbol paired with its order book,
    used both for per-participant resting counts and for the [~focus_symbol]
    depth section. A [~focus_symbol] absent from [~books] yields an empty
    depth section (empty BBO, zero sizes) rather than an error. *)
val build_snapshot
  :  t
  -> now:Time_ns.t
  -> memory:Monitor_snapshot.Memory_stats.t
  -> books:(Symbol.t * Order_book.t) list
  -> focus_symbol:Symbol.t
  -> Monitor_snapshot.t

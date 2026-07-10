(** Aggregates the live metrics behind {!Rpc_protocol.monitor_feed_rpc} and
    assembles them into a {!Dashboard_snapshot.t}.

    (Named [Metrics] rather than [Monitor] to avoid clashing with
    {!Async.Monitor}, which every [open! Async] brings into scope.)

    Like {!Latency_tracker}, this is a pure state machine: the async server
    layer feeds it observations (the [note_*]/[record_*_latency] functions,
    called from the matching loop) and, once per second, asks it to
    [build_snapshot] from inputs it supplies — the current time, the process
    memory reading, and the order books. Nothing here reads the wall clock,
    samples the GC, or touches Async, so it is unit testable in isolation.

    It owns two kinds of state:
    - a {!Latency_tracker} for submit/cancel latency, and
    - trailing one-second windows of submit and cancel timestamps per
      participant, from which order/cancel rate — and the rate-limit
      decisions in {!submit_blocked}/{!cancel_blocked} — are derived.

    Resting-order counts and book depth are {e not} tracked here — they are
    read straight from the books at snapshot time, since the books already
    are the source of truth for resting interest. *)

open! Core
open Jsip_types
open Jsip_order_book
open Jsip_protocol

type t

val create : unit -> t

(** Note that [participant] attempted a submit at [~now], adding it to their
    trailing order-rate window. Called before the block decision so that even
    a rejected submit counts toward the rate. *)
val note_submit : t -> now:Time_ns.t -> participant:Participant.t -> unit

(** As {!note_submit}, for cancels. *)
val note_cancel : t -> now:Time_ns.t -> participant:Participant.t -> unit

(** Record the measured [~latency] of an accepted submit, observed at [~now].
    Feeds only the latency tracker; rate is handled by {!note_submit}. *)
val record_submit_latency
  :  t
  -> now:Time_ns.t
  -> latency:Time_ns.Span.t
  -> unit

(** As {!record_submit_latency}, for cancels. *)
val record_cancel_latency
  :  t
  -> now:Time_ns.t
  -> latency:Time_ns.Span.t
  -> unit

(** [participant]'s live resting-order count across every book in [books],
    both sides. Read straight from the books, so it reflects the current book
    state rather than any window. *)
val resting_order_count
  :  (Symbol.t * Order_book.t) list
  -> participant:Participant.t
  -> int

(** Whether [participant]'s next submit should be rejected under [~limits],
    given their trailing submit rate and their resting-order count across
    [~books]. [Some reason] blocks (the reason feeds the [Order_reject]
    event); [None] allows. *)
val submit_blocked
  :  t
  -> now:Time_ns.t
  -> participant:Participant.t
  -> books:(Symbol.t * Order_book.t) list
  -> limits:Rate_limits.t
  -> string option

(** Whether [participant]'s next cancel should be rejected under [~limits],
    given their trailing cancel rate. [Some reason] blocks (feeding the
    [Cancel_reject] event); [None] allows. *)
val cancel_blocked
  :  t
  -> now:Time_ns.t
  -> participant:Participant.t
  -> limits:Rate_limits.t
  -> string option

(** Assemble a snapshot as of [~now].

    [~memory] is the process memory and GC reading (from [Gc.stat] in the
    async layer). [~books] is every traded symbol paired with its order book,
    used both for per-participant resting counts and for the [~focus_symbol]
    depth section. A [~focus_symbol] absent from [~books] yields an empty
    depth section (empty BBO, zero sizes) rather than an error. *)
val build_snapshot
  :  t
  -> now:Time_ns.t
  -> memory:Dashboard_snapshot.Memory_stats.t
  -> books:(Symbol.t * Order_book.t) list
  -> focus_symbol:Symbol.t
  -> Dashboard_snapshot.t

(** Per-participant rate and resting-order stats as of [~now], one entry per
    participant who is either currently submitting or has resting interest in
    [~books]. This is the [participants] section of {!build_snapshot},
    exposed on its own so it can be built and tested without a full snapshot. *)
val participant_stats
  :  t
  -> now:Time_ns.t
  -> books:(Symbol.t * Order_book.t) list
  -> Dashboard_snapshot.Participant_stats.t list

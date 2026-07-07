(** Tracks submit-order and cancel-order latency for the monitoring
    dashboard.

    Kept deliberately separate from the rest of the server logic: this module
    owns only the timing bookkeeping. It is a pure state machine — it records
    already-measured latencies (the wall-clock reads happen in the async
    server layer and are handed in as [~now] and [~latency]) and summarizes
    them on demand. No [Time_ns.now], no Async, so it is directly unit
    testable, in the same spirit as {!Controller} in the monitor.

    Each latency is retained for a trailing one-second {!window}; summaries
    ({!submit_stats}, {!cancel_stats}) describe only the samples still inside
    that window. Reads are non-destructive, so several dashboard subscribers
    can summarize the same window independently.

    {[
      let t = Latency_tracker.create () in
      Latency_tracker.record_submit t ~now ~latency:(Time_ns.Span.of_ms 3.);
      let stats = Latency_tracker.submit_stats t ~now in
      (* stats.count = 1, stats.max = 3ms *)
    ]} *)

open! Core
open Jsip_protocol

type t

(** The trailing window a sample is counted in. A sample older than this
    (relative to the [~now] passed to a query) is dropped. *)
val window : Time_ns.Span.t

val create : unit -> t

(** Record one measured submit-order latency, observed at [~now]. *)
val record_submit : t -> now:Time_ns.t -> latency:Time_ns.Span.t -> unit

(** Record one measured cancel-order latency, observed at [~now]. *)
val record_cancel : t -> now:Time_ns.t -> latency:Time_ns.Span.t -> unit

(** Summary of submit-order latencies within the {!window} ending at [~now].
    {!Dashboard_snapshot.Latency_stats.empty} when the window holds no
    samples. *)
val submit_stats : t -> now:Time_ns.t -> Dashboard_snapshot.Latency_stats.t

(** Summary of cancel-order latencies within the {!window} ending at [~now]. *)
val cancel_stats : t -> now:Time_ns.t -> Dashboard_snapshot.Latency_stats.t

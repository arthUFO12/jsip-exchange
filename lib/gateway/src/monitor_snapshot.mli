(** A single point-in-time snapshot of exchange health, streamed once per
    second over {!Rpc_protocol.monitor_feed_rpc} to the monitoring dashboard.

    One snapshot bundles every metric the dashboard displays so a subscriber
    gets a single consistent view of the exchange per tick, rather than
    reconciling several independent streams. The metrics are:

    - {{!Memory_stats} process memory and GC activity},
    - submit- and cancel-order {{!Latency_stats} latency},
    - per-participant {{!Participant_stats} order rate and resting-order
      count},
    - order-book {{!Book_depth} depth} (BBO and total resting size) for one
      focus symbol chosen by the subscriber.

    Everything here is plain data with [bin_io], so it crosses the RPC
    boundary and can be rendered directly by the frontend. *)

open! Core
open Jsip_types

(** Summary statistics for a latency distribution, computed over a trailing
    one-second window of samples. All spans are zero when [count = 0] (no
    orders in the window). See {!Latency_tracker}. *)
module Latency_stats : sig
  type t =
    { count : int (** number of samples in the window *)
    ; mean : Time_ns.Span.t
    ; p50 : Time_ns.Span.t (** median *)
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t (** tail latency *)
    ; max : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, compare, equal]

  (** All-zero stats, [count = 0]. Used for an empty window. *)
  val empty : t
end

(** Per-participant activity: how fast they are submitting and how much
    resting interest they currently have on the books. Together these surface
    an adversarial participant such as the spammer bot — a high
    [orders_per_sec] with few resting orders is the classic quote-stuffing
    signature. *)
module Participant_stats : sig
  type t =
    { participant : Participant.t
    ; orders_per_sec : float
    (** submissions in the trailing one-second window *)
    ; resting_order_count : int
    (** open orders across all symbols right now *)
    }
  [@@deriving sexp, bin_io, compare, equal]
end

(** Order-book depth for a single symbol: the live BBO plus the total resting
    size on each side, summed across every price level (not just the best). *)
module Book_depth : sig
  type t =
    { symbol : Symbol.t
    ; bbo : Bbo.t
    ; total_resting_bid_size : Size.t
    ; total_resting_ask_size : Size.t
    }
  [@@deriving sexp, bin_io, compare, equal]
end

(** Process memory and garbage-collector activity, read from [Gc.stat] once
    per snapshot. All figures are in machine words, not bytes. [live_words]
    is the currently-reachable heap (not total capacity); the [_words]
    allocation counters and collection counts are cumulative over the process
    lifetime, so their {e change} between snapshots is what reveals
    allocation pressure and GC frequency. *)
module Memory_stats : sig
  type t =
    { live_words : int (** currently-reachable heap, in words *)
    ; major_words : float (** cumulative words allocated in the major heap *)
    ; minor_words : float (** cumulative words allocated in the minor heap *)
    ; major_collections : int (** completed major GC cycles *)
    ; minor_collections : int (** completed minor collections *)
    }
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { sampled_at : Time_ns.t
  ; memory : Memory_stats.t
  ; submit_latency : Latency_stats.t
  ; cancel_latency : Latency_stats.t
  ; participants : Participant_stats.t list
  ; book_depth : Book_depth.t (** depth for the subscriber's focus symbol *)
  }
[@@deriving sexp, bin_io, compare, equal]



val memory_stats_of_gc : Gc.Stat.t -> Memory_stats.t





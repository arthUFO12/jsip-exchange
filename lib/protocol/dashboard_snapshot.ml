open! Core
open Jsip_types

module Latency_stats = struct
  type t =
    { count : int
    ; mean : Time_ns.Span.t
    ; p50 : Time_ns.Span.t
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    ; max : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, compare, equal]

  let empty =
    { count = 0
    ; mean = Time_ns.Span.zero
    ; p50 = Time_ns.Span.zero
    ; p90 = Time_ns.Span.zero
    ; p99 = Time_ns.Span.zero
    ; max = Time_ns.Span.zero
    }
  ;;
end

module Participant_stats = struct
  type t =
    { participant : Participant.t
    ; orders_per_sec : float
    ; resting_order_count : int
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Book_depth = struct
  type t =
    { symbol : Symbol.t
    ; bbo : Bbo.t
    ; total_resting_bid_size : Size.t
    ; total_resting_ask_size : Size.t
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Memory_stats = struct
  type t =
    { live_words : int
    ; major_words : float
    ; minor_words : float
    ; major_collections : int
    ; minor_collections : int
    }
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { sampled_at : Time_ns.t
  ; memory : Memory_stats.t
  ; submit_latency : Latency_stats.t
  ; cancel_latency : Latency_stats.t
  ; participants : Participant_stats.t list
  ; book_depth : Book_depth.t
  }
[@@deriving sexp, bin_io, compare, equal]

let memory_stats_of_gc (gc_stat : Gc.Stat.t) : Memory_stats.t =
  { live_words = Gc.Stat.live_words gc_stat
  ; major_words = Gc.Stat.major_words gc_stat
  ; minor_words = Gc.Stat.minor_words gc_stat
  ; major_collections = Gc.Stat.major_collections gc_stat
  ; minor_collections = Gc.Stat.minor_collections gc_stat
  }
;;

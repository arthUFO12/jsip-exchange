open! Core
open Jsip_types
module Dashboard_snapshot = Jsip_protocol.Dashboard_snapshot

module Pane = struct
  type t =
    | Memory
    | Latency
    | Participants
    | Book_depth
  [@@deriving sexp, compare, equal]

  let all = [ Memory; Latency; Participants; Book_depth ]

  let to_string = function
    | Memory -> "memory"
    | Latency -> "latency"
    | Participants -> "participants"
    | Book_depth -> "book depth"
  ;;

  let hotkey = function
    | Memory -> '1'
    | Latency -> '2'
    | Participants -> '3'
    | Book_depth -> '4'
  ;;

  let of_hotkey = function
    | '1' -> Some Memory
    | '2' -> Some Latency
    | '3' -> Some Participants
    | '4' -> Some Book_depth
    | _ -> None
  ;;
end

module Focus = struct
  type t =
    | All
    | Single of Pane.t
  [@@deriving sexp, compare, equal]

  let hotkey = function All -> '0' | Single pane -> Pane.hotkey pane

  let of_hotkey = function
    | '0' -> Some All
    | c -> Option.map (Pane.of_hotkey c) ~f:(fun pane -> Single pane)
  ;;
end

module Display = struct
  module Memory_panel = struct
    type t =
      { live_words : int
      ; major_words : float
      ; minor_words : float
      ; major_collections : int
      ; minor_collections : int
      ; live_words_trend : int list
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Latency_panel = struct
    type row =
      { label : string
      ; count : int
      ; mean_ms : float
      ; p50_ms : float
      ; p90_ms : float
      ; p99_ms : float
      ; max_ms : float
      }
    [@@deriving sexp_of, compare, equal]

    type t =
      { submit : row
      ; cancel : row
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Participant_row = struct
    type t =
      { name : string
      ; orders_per_sec : float
      ; resting_order_count : int
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Book_depth_panel = struct
    type t =
      { symbol : Symbol.t
      ; bbo : Bbo.t
      ; total_bid_size : Size.t
      ; total_ask_size : Size.t
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Tab = struct
    type t =
      { pane : Pane.t
      ; label : string
      ; focused : bool
      }
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { title : string
    ; status : string
    ; focus : Focus.t
    ; show_all : bool
    ; tabs : Tab.t list
    ; memory : Memory_panel.t option
    ; latency : Latency_panel.t option
    ; participants : Participant_row.t list
    ; book_depth : Book_depth_panel.t option
    ; sample_count : int
    }
  [@@deriving sexp_of, compare, equal]
end

(* The window is stored oldest-first, so pruning drops from the front. *)
type t =
  { history : Dashboard_snapshot.t list
  ; focus : Focus.t
  }

let create () = { history = []; focus = All }
let window = Time_ns.Span.of_min 1.

(* Drop snapshots that fall outside the trailing [window] ending at [newest]
   (the [sampled_at] of the snapshot just appended). The list is oldest-first
   and time-ordered, so the expired snapshots are exactly a prefix.

   TODO(human) Return the sublist of [history] still inside the window: keep
   every snapshot whose age relative to [newest] is at most [window], drop
   the rest. Because [history] is oldest-first and ordered by [sampled_at],
   the too-old snapshots form a prefix, so [List.drop_while] fits well.

   The age of a snapshot [s] is [Time_ns.diff newest s.sampled_at]; it is
   expired when that span is strictly greater than [window] (a snapshot
   exactly [window] old is kept — the same [>]-boundary rule as
   [Latency_tracker.prune] in the backend). Handy pieces:
   [List.drop_while ~f], [Time_ns.diff], [Time_ns.Span.( > )]. *)
let prune (history : Dashboard_snapshot.t list) ~(newest : Time_ns.t) =
  let should_prune (snapshot : Dashboard_snapshot.t) =
    Time_ns.( < ) snapshot.sampled_at (Time_ns.sub newest window)
  in
  List.drop_while history ~f:should_prune
;;

let feed_snapshot t (snapshot : Dashboard_snapshot.t) =
  let history = t.history @ [ snapshot ] in
  { t with history = prune history ~newest:snapshot.sampled_at }
;;

let select t focus = { t with focus }
let title = "JSIP Exchange Dashboard"
let ms span = Time_ns.Span.to_ms span

let latency_row label (stats : Dashboard_snapshot.Latency_stats.t)
  : Display.Latency_panel.row
  =
  { label
  ; count = stats.count
  ; mean_ms = ms stats.mean
  ; p50_ms = ms stats.p50
  ; p90_ms = ms stats.p90
  ; p99_ms = ms stats.p99
  ; max_ms = ms stats.max
  }
;;

let memory_panel history (memory : Dashboard_snapshot.Memory_stats.t)
  : Display.Memory_panel.t
  =
  { live_words = memory.live_words
  ; major_words = memory.major_words
  ; minor_words = memory.minor_words
  ; major_collections = memory.major_collections
  ; minor_collections = memory.minor_collections
  ; live_words_trend =
      List.map history ~f:(fun (s : Dashboard_snapshot.t) ->
        s.memory.live_words)
  }
;;

let participant_rows
  (participants : Dashboard_snapshot.Participant_stats.t list)
  =
  List.map
    participants
    ~f:(fun (p : Dashboard_snapshot.Participant_stats.t) ->
      { Display.Participant_row.name = Participant.to_string p.participant
      ; orders_per_sec = p.orders_per_sec
      ; resting_order_count = p.resting_order_count
      })
;;

let book_depth_panel (depth : Dashboard_snapshot.Book_depth.t)
  : Display.Book_depth_panel.t
  =
  { symbol = depth.symbol
  ; bbo = depth.bbo
  ; total_bid_size = depth.total_resting_bid_size
  ; total_ask_size = depth.total_resting_ask_size
  }
;;

let tabs focus =
  List.map Pane.all ~f:(fun pane ->
    { Display.Tab.pane
    ; label = Pane.to_string pane
    ; focused =
        (match (focus : Focus.t) with
         | All -> false
         | Single p -> Pane.equal p pane)
    })
;;

let display t : Display.t =
  let show_all = match t.focus with All -> true | Single _ -> false in
  let sample_count = List.length t.history in
  let tabs = tabs t.focus in
  match List.last t.history with
  | None ->
    { title
    ; status = "waiting for first snapshot"
    ; focus = t.focus
    ; show_all
    ; tabs
    ; memory = None
    ; latency = None
    ; participants = []
    ; book_depth = None
    ; sample_count
    }
  | Some (latest : Dashboard_snapshot.t) ->
    { title
    ; status = [%string "%{sample_count#Int} samples in the last 60s"]
    ; focus = t.focus
    ; show_all
    ; tabs
    ; memory = Some (memory_panel t.history latest.memory)
    ; latency =
        Some
          { submit = latency_row "submit" latest.submit_latency
          ; cancel = latency_row "cancel" latest.cancel_latency
          }
    ; participants = participant_rows latest.participants
    ; book_depth = Some (book_depth_panel latest.book_depth)
    ; sample_count
    }
;;

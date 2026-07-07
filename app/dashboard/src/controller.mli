(** The dashboard's pure state machine.

    A [Controller.t] holds a rolling one-minute window of
    {!Jsip_protocol.Dashboard_snapshot.t}s streamed from the exchange, plus
    which {!Pane} the operator has brought to the forefront. [feed_snapshot]
    and [select] are pure transitions; [display] projects the current state
    into a plain-data {!Display.t} that the bonsai_web layer ({!View})
    renders.

    Like the monitor's [Controller] ([app/monitor/src/controller.mli]), this
    is deliberately free of any bonsai type, so it is unit-testable as plain
    data. *)

open! Core
open Jsip_types

(** The four content panes the dashboard can show. *)
module Pane : sig
  type t =
    | Memory
    | Latency
    | Participants
    | Book_depth
  [@@deriving sexp, compare, equal]

  (** All panes in canonical left-to-right tab order. *)
  val all : t list

  (** Human label, e.g. [Book_depth] renders as ["book depth"]. *)
  val to_string : t -> string

  (** The number key that brings this pane forward: ['1'] .. ['4']. *)
  val hotkey : t -> char

  (** Inverse of {!hotkey}; [None] for any other character. *)
  val of_hotkey : char -> t option
end

(** Which pane is in the forefront, or [All] for the overview that shows
    every pane at once. *)
module Focus : sig
  type t =
    | All
    | Single of Pane.t
  [@@deriving sexp, compare, equal]

  (** ['0'] selects [All]; ['1']..['4'] select the corresponding {!Pane}. *)
  val hotkey : t -> char

  val of_hotkey : char -> t option
end

(** The plain-data view the bonsai_web layer reads. Decoupled from any bonsai
    type so the controller stays fully testable. *)
module Display : sig
  module Memory_panel : sig
    type t =
      { live_words : int
      ; major_words : float
      ; minor_words : float
      ; major_collections : int
      ; minor_collections : int
      ; live_words_trend : int list
      (** [live_words] across the window, oldest-first, for a sparkline. *)
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Latency_panel : sig
    (** One latency distribution, spans converted to milliseconds for
        display. *)
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

  module Participant_row : sig
    type t =
      { name : string
      ; orders_per_sec : float
      ; resting_order_count : int
      }
    [@@deriving sexp_of, compare, equal]
  end

  module Book_depth_panel : sig
    type t =
      { symbol : Symbol.t
      ; bbo : Bbo.t
      ; total_bid_size : Size.t
      ; total_ask_size : Size.t
      }
    [@@deriving sexp_of, compare, equal]
  end

  (** One entry in the pane switcher. [focused] is [true] for the pane the
      operator has brought to the forefront. *)
  module Tab : sig
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
    (** ["waiting for first snapshot"] before any data, else a short summary
        of how many samples the window holds. *)
    ; focus : Focus.t
    ; show_all : bool (** [true] when {!field-focus} is [All]. *)
    ; tabs : Tab.t list
    ; memory : Memory_panel.t option (** [None] until the first snapshot. *)
    ; latency : Latency_panel.t option
    ; participants : Participant_row.t list
    ; book_depth : Book_depth_panel.t option
    ; sample_count : int
    }
  [@@deriving sexp_of, compare, equal]
end

type t

val create : unit -> t

(** The trailing window the dashboard displays: the last minute of snapshots. *)
val window : Time_ns.Span.t

(** Append [snapshot] to the window, evicting any snapshot whose [sampled_at]
    is more than {!window} older than [snapshot]'s. *)
val feed_snapshot : t -> Jsip_protocol.Dashboard_snapshot.t -> t

(** Bring a pane (or the [All] overview) to the forefront. *)
val select : t -> Focus.t -> t

(** Project the current window into the renderable {!Display.t}. *)
val display : t -> Display.t

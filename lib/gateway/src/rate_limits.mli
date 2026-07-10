(** Per-participant limits the exchange enforces on the matching loop.

    A participant's requests are rejected (via {!Jsip_types.Exchange_event}
    reject events on their session feed) once they exceed any of these:

    - [max_resting_orders] — open orders across every symbol at once, counted
      straight from the books;
    - [max_submits_per_sec] — submit-order requests in the trailing second;
    - [max_cancels_per_sec] — cancel-order requests in the trailing second.

    These are on by default (see {!default}); the server binary surfaces each
    as a command-line flag. The rate windows are the same trailing one-second
    windows {!Metrics} already keeps for the dashboard, so a participant's
    dashboard rate and their limit are measured over the identical slice of
    time. *)

open! Core

type t =
  { max_resting_orders : int
  ; max_submits_per_sec : int
  ; max_cancels_per_sec : int
  }
[@@deriving sexp_of]

(** Generous defaults, chosen to sit well above normal bot and market-maker
    traffic while still catching a participant that floods the book or the
    matching loop. *)
val default : t

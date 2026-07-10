open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway
open Jsip_test_harness

let now = Time_ns.epoch
let at ms = Time_ns.add now (Time_ns.Span.of_ms ms)

let books_of harness symbols =
  List.filter_map symbols ~f:(fun symbol ->
    Matching_engine.book (Harness.engine harness) symbol
    |> Option.map ~f:(fun book -> symbol, book))
;;

(* Distinct values per field so the expect output shows each one is carried
   through [build_snapshot] to the right place. *)
let test_memory : Dashboard_snapshot.Memory_stats.t =
  { live_words = 4096
  ; major_words = 100.
  ; minor_words = 250.
  ; major_collections = 3
  ; minor_collections = 12
  }
;;

let show (snap : Dashboard_snapshot.t) =
  let mem = snap.memory in
  printf
    "memory: live_words=%d major_words=%.0f minor_words=%.0f \
     major_collections=%d minor_collections=%d\n"
    mem.live_words
    mem.major_words
    mem.minor_words
    mem.major_collections
    mem.minor_collections;
  printf "participants:\n";
  List.iter
    snap.participants
    ~f:(fun (p : Dashboard_snapshot.Participant_stats.t) ->
      printf
        "  %s rate=%.1f resting=%d\n"
        (Participant.to_string p.participant)
        p.orders_per_sec
        p.resting_order_count);
  let depth = snap.book_depth in
  printf
    "depth %s bbo=%s total_bid=%s total_ask=%s\n"
    (Symbol.to_string depth.symbol)
    (Bbo.to_string depth.bbo)
    (Size.to_string depth.total_resting_bid_size)
    (Size.to_string depth.total_resting_ask_size)
;;

let%expect_test "resting counts (all symbols) and focus-symbol depth" =
  let h = Harness.create ~symbols:[ Harness.aapl; Harness.tsla ] () in
  Harness.submit_quiet_
    h
    ~participant:Harness.alice
    (Harness.buy ~price_cents:10000 ~size:50 ());
  Harness.submit_quiet_
    h
    ~participant:Harness.bob
    (Harness.buy ~price_cents:9900 ~size:30 ());
  Harness.submit_quiet_
    h
    ~participant:Harness.alice
    (Harness.sell ~price_cents:10100 ~size:20 ());
  (* A resting order on a different symbol still counts toward alice's total. *)
  Harness.submit_quiet_
    h
    ~participant:Harness.alice
    (Harness.buy ~price_cents:5000 ~size:10 ~symbol:Harness.tsla ());
  let m = Metrics.create () in
  let snap =
    Metrics.build_snapshot
      m
      ~now
      ~memory:test_memory
      ~books:(books_of h [ Harness.aapl; Harness.tsla ])
      ~focus_symbol:Harness.aapl
  in
  show snap;
  [%expect
    {|
    memory: live_words=4096 major_words=100 minor_words=250 major_collections=3 minor_collections=12
    participants:
      Alice rate=0.0 resting=3
      Bob rate=0.0 resting=1
    depth AAPL bbo=$100.00 x50 / $101.00 x20 total_bid=80 total_ask=20
    |}]
;;

let make_limits
  ?(max_resting_orders = 1000)
  ?(max_submits_per_sec = 100)
  ?(max_cancels_per_sec = 100)
  ()
  : Rate_limits.t
  =
  { max_resting_orders; max_submits_per_sec; max_cancels_per_sec }
;;

(* Print only the block/allow decision, not the reason text, so these tests
   stay agnostic to how the reason strings are worded. *)
let show_decision label decision =
  match (decision : string option) with
  | None -> printf "%s: allowed\n" label
  | Some (_reason : string) -> printf "%s: blocked\n" label
;;

(* When it matters that the {e right} limit is reported (a block can trip on
   either the resting or the rate limit), print the reason too. *)
let show_reason label decision =
  match (decision : string option) with
  | None -> printf "%s: allowed\n" label
  | Some reason -> printf "%s: blocked (%s)\n" label reason
;;

let%expect_test "submit_blocked: submit rate over the limit is rejected" =
  let h = Harness.create ~symbols:[ Harness.aapl ] () in
  let m = Metrics.create () in
  let limits = make_limits ~max_submits_per_sec:3 () in
  let books = books_of h [ Harness.aapl ] in
  (* Charlie fires five submits inside the window: over the 3/s limit. *)
  List.iter [ 0.; 10.; 20.; 30.; 40. ] ~f:(fun offset ->
    Metrics.note_submit m ~now:(at offset) ~participant:Harness.charlie);
  show_decision
    "charlie (5 submits, limit 3)"
    (Metrics.submit_blocked
       m
       ~now:(at 40.)
       ~participant:Harness.charlie
       ~books
       ~limits);
  (* Alice has neither submitted nor rested: comfortably under both limits. *)
  show_decision
    "alice (0 submits)"
    (Metrics.submit_blocked
       m
       ~now:(at 40.)
       ~participant:Harness.alice
       ~books
       ~limits);
  [%expect
    {|
    charlie (5 submits, limit 3): blocked
    alice (0 submits): allowed
    |}]
;;

let%expect_test "submit_blocked: too many resting orders is rejected" =
  let h = Harness.create ~symbols:[ Harness.aapl ] () in
  let m = Metrics.create () in
  let limits = make_limits ~max_resting_orders:2 () in
  (* Four non-crossing buys all rest, putting Alice over the limit of 2. *)
  List.iter [ 10000; 9900; 9800; 9700 ] ~f:(fun price_cents ->
    Harness.submit_quiet_
      h
      ~participant:Harness.alice
      (Harness.buy ~price_cents ~size:10 ()));
  (* Bob rests a single order: under the limit. *)
  Harness.submit_quiet_
    h
    ~participant:Harness.bob
    (Harness.buy ~price_cents:9500 ~size:10 ());
  let books = books_of h [ Harness.aapl ] in
  (* Alice's calm submit rate means a block here must cite the resting limit,
     not the rate limit. *)
  show_reason
    "alice (4 resting, limit 2)"
    (Metrics.submit_blocked m ~now ~participant:Harness.alice ~books ~limits);
  show_reason
    "bob (1 resting, limit 2)"
    (Metrics.submit_blocked m ~now ~participant:Harness.bob ~books ~limits);
  [%expect
    {|
    alice (4 resting, limit 2): blocked (resting order limit exceeded: 4 > 2)
    bob (1 resting, limit 2): allowed
    |}]
;;

let%expect_test "cancel_blocked: cancel rate over the limit is rejected" =
  let m = Metrics.create () in
  let limits = make_limits ~max_cancels_per_sec:3 () in
  (* Charlie fires five cancels inside the window: over the 3/s limit. *)
  List.iter [ 0.; 10.; 20.; 30.; 40. ] ~f:(fun offset ->
    Metrics.note_cancel m ~now:(at offset) ~participant:Harness.charlie);
  show_decision
    "charlie (5 cancels, limit 3)"
    (Metrics.cancel_blocked
       m
       ~now:(at 40.)
       ~participant:Harness.charlie
       ~limits);
  (* Alice hasn't cancelled at all. *)
  show_decision
    "alice (0 cancels)"
    (Metrics.cancel_blocked
       m
       ~now:(at 40.)
       ~participant:Harness.alice
       ~limits);
  [%expect
    {|
    charlie (5 cancels, limit 3): blocked
    alice (0 cancels): allowed
    |}]
;;

let%expect_test "order rate reflects submissions in the trailing window" =
  let h = Harness.create ~symbols:[ Harness.aapl ] () in
  let m = Metrics.create () in
  (* A spammer fires three orders within the last second but rests nothing. *)
  List.iter [ 0.; 100.; 200. ] ~f:(fun offset ->
    Metrics.note_submit m ~now:(at offset) ~participant:Harness.charlie);
  let snap =
    Metrics.build_snapshot
      m
      ~now:(at 200.)
      ~memory:test_memory
      ~books:(books_of h [ Harness.aapl ])
      ~focus_symbol:Harness.aapl
  in
  show snap;
  [%expect
    {|
    memory: live_words=4096 major_words=100 minor_words=250 major_collections=3 minor_collections=12
    participants:
      Charlie rate=3.0 resting=0
    depth AAPL bbo=- / - total_bid=0 total_ask=0
    |}]
;;

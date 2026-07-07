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
let test_memory : Monitor_snapshot.Memory_stats.t =
  { live_words = 4096
  ; major_words = 100.
  ; minor_words = 250.
  ; major_collections = 3
  ; minor_collections = 12
  }
;;

let show (snap : Monitor_snapshot.t) =
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
    ~f:(fun (p : Monitor_snapshot.Participant_stats.t) ->
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

let%expect_test "order rate reflects submissions in the trailing window" =
  let h = Harness.create ~symbols:[ Harness.aapl ] () in
  let m = Metrics.create () in
  (* A spammer fires three orders within the last second but rests nothing. *)
  List.iter [ 0.; 100.; 200. ] ~f:(fun offset ->
    Metrics.record_submit
      m
      ~now:(at offset)
      ~latency:(Time_ns.Span.of_ms 1.)
      ~participant:Harness.charlie);
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

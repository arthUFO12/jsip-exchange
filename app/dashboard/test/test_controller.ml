open! Core
open Jsip_types
open Jsip_gateway
open Jsip_dashboard
open Jsip_test_harness

let t0 = Time_ns.epoch
let at_sec s = Time_ns.add t0 (Time_ns.Span.of_sec s)

(* All percentiles collapse to one value; the controller only reshapes these
   numbers, so a single knob per snapshot keeps the expect output legible. *)
let latency ~count ~ms : Dashboard_snapshot.Latency_stats.t =
  let span = Time_ns.Span.of_ms ms in
  { count; mean = span; p50 = span; p90 = span; p99 = span; max = span }
;;

let memory ~live_words : Dashboard_snapshot.Memory_stats.t =
  { live_words
  ; major_words = 0.
  ; minor_words = 0.
  ; major_collections = 0
  ; minor_collections = 0
  }
;;

let book_depth : Dashboard_snapshot.Book_depth.t =
  { symbol = Harness.aapl
  ; bbo = Bbo.empty
  ; total_resting_bid_size = Size.zero
  ; total_resting_ask_size = Size.zero
  }
;;

let snapshot ~at ~live_words ?(participants = []) () : Dashboard_snapshot.t =
  { sampled_at = at
  ; memory = memory ~live_words
  ; submit_latency = latency ~count:0 ~ms:0.
  ; cancel_latency = latency ~count:0 ~ms:0.
  ; participants
  ; book_depth
  }
;;

let show c = print_s [%sexp (Controller.display c : Controller.Display.t)]

let%expect_test "initial state is waiting for the first snapshot" =
  show (Controller.create ());
  [%expect
    {|
    ((title "JSIP Exchange Dashboard") (status "waiting for first snapshot")
     (focus All) (show_all true)
     (tabs
      (((pane Memory) (label memory) (focused false))
       ((pane Latency) (label latency) (focused false))
       ((pane Participants) (label participants) (focused false))
       ((pane Book_depth) (label "book depth") (focused false))))
     (memory ()) (latency ()) (participants ()) (book_depth ()) (sample_count 0))
    |}]
;;

let%expect_test "a single snapshot populates every panel" =
  let alice : Dashboard_snapshot.Participant_stats.t =
    { participant = Harness.alice
    ; orders_per_sec = 2.
    ; resting_order_count = 3
    }
  in
  let c =
    Controller.feed_snapshot
      (Controller.create ())
      (snapshot ~at:t0 ~live_words:100 ~participants:[ alice ] ())
  in
  show c;
  [%expect
    {|
    ((title "JSIP Exchange Dashboard") (status "1 samples in the last 60s")
     (focus All) (show_all true)
     (tabs
      (((pane Memory) (label memory) (focused false))
       ((pane Latency) (label latency) (focused false))
       ((pane Participants) (label participants) (focused false))
       ((pane Book_depth) (label "book depth") (focused false))))
     (memory
      (((live_words 100) (major_words 0) (minor_words 0) (major_collections 0)
        (minor_collections 0) (live_words_trend (100)))))
     (latency
      (((submit
         ((label submit) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0)))
        (cancel
         ((label cancel) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0))))))
     (participants (((name Alice) (orders_per_sec 2) (resting_order_count 3))))
     (book_depth
      (((symbol AAPL) (bbo ((bid ()) (ask ()))) (total_bid_size 0)
        (total_ask_size 0))))
     (sample_count 1))
    |}]
;;

let%expect_test "the window keeps only the last minute of snapshots" =
  let c = Controller.create () in
  let c =
    Controller.feed_snapshot c (snapshot ~at:(at_sec 0.) ~live_words:100 ())
  in
  let c =
    Controller.feed_snapshot c (snapshot ~at:(at_sec 30.) ~live_words:200 ())
  in
  (* At 61s the 0s snapshot is 61s old (> 60s) and evicted; the 30s and 61s
     snapshots survive, so the trend holds [200; 300]. *)
  let c =
    Controller.feed_snapshot c (snapshot ~at:(at_sec 61.) ~live_words:300 ())
  in
  show c;
  [%expect
    {|
    ((title "JSIP Exchange Dashboard") (status "2 samples in the last 60s")
     (focus All) (show_all true)
     (tabs
      (((pane Memory) (label memory) (focused false))
       ((pane Latency) (label latency) (focused false))
       ((pane Participants) (label participants) (focused false))
       ((pane Book_depth) (label "book depth") (focused false))))
     (memory
      (((live_words 300) (major_words 0) (minor_words 0) (major_collections 0)
        (minor_collections 0) (live_words_trend (200 300)))))
     (latency
      (((submit
         ((label submit) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0)))
        (cancel
         ((label cancel) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0))))))
     (participants ())
     (book_depth
      (((symbol AAPL) (bbo ((bid ()) (ask ()))) (total_bid_size 0)
        (total_ask_size 0))))
     (sample_count 2))
    |}]
;;

let%expect_test "select brings a pane to the forefront" =
  let c =
    Controller.feed_snapshot
      (Controller.create ())
      (snapshot ~at:t0 ~live_words:100 ())
  in
  let c = Controller.select c (Single Latency) in
  show c;
  [%expect
    {|
    ((title "JSIP Exchange Dashboard") (status "1 samples in the last 60s")
     (focus (Single Latency)) (show_all false)
     (tabs
      (((pane Memory) (label memory) (focused false))
       ((pane Latency) (label latency) (focused true))
       ((pane Participants) (label participants) (focused false))
       ((pane Book_depth) (label "book depth") (focused false))))
     (memory
      (((live_words 100) (major_words 0) (minor_words 0) (major_collections 0)
        (minor_collections 0) (live_words_trend (100)))))
     (latency
      (((submit
         ((label submit) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0)))
        (cancel
         ((label cancel) (count 0) (mean_ms 0) (p50_ms 0) (p90_ms 0) (p99_ms 0)
          (max_ms 0))))))
     (participants ())
     (book_depth
      (((symbol AAPL) (bbo ((bid ()) (ask ()))) (total_bid_size 0)
        (total_ask_size 0))))
     (sample_count 1))
    |}]
;;

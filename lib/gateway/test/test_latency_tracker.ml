open! Core
open Jsip_gateway

(* Print a latency summary as integer milliseconds so the expect output stays
   stable and readable (raw [Time_ns.Span.t] sexps auto-scale their unit). *)
let show (stats : Monitor_snapshot.Latency_stats.t) =
  let ms span = Float.iround_nearest_exn (Time_ns.Span.to_ms span) in
  printf
    "count=%d mean=%dms p50=%dms p99=%dms max=%dms\n"
    stats.count
    (ms stats.mean)
    (ms stats.p50)
    (ms stats.p99)
    (ms stats.max)
;;

let t0 = Time_ns.epoch
let at ms = Time_ns.add t0 (Time_ns.Span.of_ms ms)
let ms x = Time_ns.Span.of_ms x

let%expect_test "empty window summarizes to zeros" =
  let t = Latency_tracker.create () in
  show (Latency_tracker.submit_stats t ~now:t0);
  [%expect {| count=0 mean=0ms p50=0ms p99=0ms max=0ms |}]
;;

let%expect_test "summary over three submit samples" =
  let t = Latency_tracker.create () in
  Latency_tracker.record_submit t ~now:t0 ~latency:(ms 1.);
  Latency_tracker.record_submit t ~now:t0 ~latency:(ms 2.);
  Latency_tracker.record_submit t ~now:t0 ~latency:(ms 3.);
  show (Latency_tracker.submit_stats t ~now:t0);
  (* mean = 2ms; p50 = sorted[round(0.5*2)] = sorted[1] = 2ms; p99 =
     sorted[round(0.99*2)] = sorted[2] = 3ms; max = 3ms *)
  [%expect {| count=3 mean=2ms p50=2ms p99=3ms max=3ms |}]
;;

let%expect_test "samples outside the trailing window are dropped" =
  let t = Latency_tracker.create () in
  Latency_tracker.record_submit t ~now:(at 0.) ~latency:(ms 5.);
  Latency_tracker.record_submit t ~now:(at 500.) ~latency:(ms 7.);
  (* At 500ms both are within the 1s window. *)
  show (Latency_tracker.submit_stats t ~now:(at 500.));
  [%expect {| count=2 mean=6ms p50=7ms p99=7ms max=7ms |}];
  (* At 1500ms the first (age 1500ms) is expired, the second (age 1000ms) is
     exactly at the window edge and kept. *)
  show (Latency_tracker.submit_stats t ~now:(at 1500.));
  [%expect {| count=1 mean=7ms p50=7ms p99=7ms max=7ms |}];
  (* At 2000ms both are expired. *)
  show (Latency_tracker.submit_stats t ~now:(at 2000.));
  [%expect {| count=0 mean=0ms p50=0ms p99=0ms max=0ms |}]
;;

let%expect_test "submit and cancel latencies are tracked independently" =
  let t = Latency_tracker.create () in
  Latency_tracker.record_submit t ~now:t0 ~latency:(ms 4.);
  Latency_tracker.record_cancel t ~now:t0 ~latency:(ms 9.);
  show (Latency_tracker.submit_stats t ~now:t0);
  [%expect {| count=1 mean=4ms p50=4ms p99=4ms max=4ms |}];
  show (Latency_tracker.cancel_stats t ~now:t0);
  [%expect {| count=1 mean=9ms p50=9ms p99=9ms max=9ms |}]
;;

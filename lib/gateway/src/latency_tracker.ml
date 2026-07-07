open! Core
open Jsip_protocol

(* One measured latency and the wall-clock instant it was observed. The
   instant is what lets us decide, later, whether the sample is still inside
   the trailing [window]. *)
module Sample = struct
  type t =
    { observed_at : Time_ns.t
    ; latency : Time_ns.Span.t
    }
end

(* Samples are held oldest-first in a queue per kind, so pruning always
   happens from the front. *)
type t =
  { submit : Sample.t Queue.t
  ; cancel : Sample.t Queue.t
  }

let window = Time_ns.Span.of_sec 1.
let create () = { submit = Queue.create (); cancel = Queue.create () }

(* Drop samples that fall outside the trailing [window] ending at [now]. The
   queue is ordered oldest-first, so we discard from the front until the
   oldest surviving sample is within the window. *)
let prune queue ~now =
  let is_expired (sample : Sample.t) =
    Time_ns.Span.( > ) (Time_ns.diff now sample.observed_at) window
  in
  let rec prune_helper () =
    if (not (Queue.is_empty queue)) && is_expired (Queue.peek_exn queue)
    then (
      ignore (Queue.dequeue_exn queue);
      prune_helper ())
    else ()
  in
  prune_helper ()
;;

let record queue ~now ~latency =
  Queue.enqueue queue { Sample.observed_at = now; latency };
  prune queue ~now
;;

let record_submit t ~now ~latency = record t.submit ~now ~latency
let record_cancel t ~now ~latency = record t.cancel ~now ~latency

(* Summarize the latencies in the trailing window into count/mean/max plus
   nearest-rank p50/p90/p99. [latencies] arrives in insertion order; we sort
   once and read every order statistic off the sorted list. An empty window
   summarizes to [Dashboard_snapshot.Latency_stats.empty]. *)
let summarize (latencies : Time_ns.Span.t list)
  : Dashboard_snapshot.Latency_stats.t
  =
  match latencies with
  | [] -> Dashboard_snapshot.Latency_stats.empty
  | _ :: _ ->
    let count = List.length latencies in
    let mean =
      Time_ns.Span.( / )
        (List.fold latencies ~init:Time_ns.Span.zero ~f:(fun sum latency ->
           Time_ns.Span.( + ) sum latency))
        (Float.of_int count)
    in
    let sorted_list = List.sort latencies ~compare:Time_ns.Span.compare in
    let max = List.last_exn sorted_list in
    let choose_percentile percentile =
      let idx =
        percentile /. 100. *. Float.of_int (count - 1)
        |> Float.round_nearest
        |> Int.of_float
      in
      List.nth sorted_list idx |> Option.value_exn
    in
    let p50, p90, p99 =
      choose_percentile 50., choose_percentile 90., choose_percentile 99.
    in
    { count; mean; max; p50; p90; p99 }
;;

let latencies_in_window queue ~now =
  prune queue ~now;
  Queue.to_list queue
  |> List.map ~f:(fun (sample : Sample.t) -> sample.latency)
;;

let submit_stats t ~now = summarize (latencies_in_window t.submit ~now)
let cancel_stats t ~now = summarize (latencies_in_window t.cancel ~now)

open! Core
open Jsip_types
open Jsip_order_book
open Jsip_protocol

type t =
  { latency : Latency_tracker.t
  ; (* Per participant, the wall-clock instants of their recent submissions,
       oldest-first. Pruned to [Latency_tracker.window] so its length is the
       submission count in the trailing window. *)
    order_events : Time_ns.t Queue.t Participant.Table.t
  ; (* The same, for cancels — added so cancels/sec can be rate-limited per
       participant. The dashboard's latency view aggregates cancels across
       everyone, but the limiter needs per-participant attribution. *)
    cancel_events : Time_ns.t Queue.t Participant.Table.t
  }

let create () =
  { latency = Latency_tracker.create ()
  ; order_events = Participant.Table.create ()
  ; cancel_events = Participant.Table.create ()
  }
;;

(* Order rate and latency share the same trailing window, so a snapshot's
   metrics all describe the same slice of time. *)
let window = Latency_tracker.window

let prune_events queue ~now =
  let is_expired instant =
    Time_ns.Span.( > ) (Time_ns.diff now instant) window
  in
  while (not (Queue.is_empty queue)) && is_expired (Queue.peek_exn queue) do
    ignore (Queue.dequeue_exn queue : Time_ns.t)
  done
;;

(* Rate accounting and latency accounting are recorded separately because the
   matching loop needs them at different moments: it notes the attempt up
   front (so a rejected request still counts toward the rate), but only
   measures latency once an accepted request has finished. *)

let note_event table ~now ~participant =
  let queue = Hashtbl.find_or_add table participant ~default:Queue.create in
  Queue.enqueue queue now;
  prune_events queue ~now
;;

let note_submit t ~now ~participant =
  note_event t.order_events ~now ~participant
;;

let note_cancel t ~now ~participant =
  note_event t.cancel_events ~now ~participant
;;

let record_submit_latency t ~now ~latency =
  Latency_tracker.record_submit t.latency ~now ~latency
;;

let record_cancel_latency t ~now ~latency =
  Latency_tracker.record_cancel t.latency ~now ~latency
;;

let rate_in_window table ~now ~participant =
  match Hashtbl.find table participant with
  | None -> 0.
  | Some queue ->
    prune_events queue ~now;
    Float.of_int (Queue.length queue) /. Time_ns.Span.to_sec window
;;

let submit_rate t ~now ~participant =
  rate_in_window t.order_events ~now ~participant
;;

let cancel_rate t ~now ~participant =
  rate_in_window t.cancel_events ~now ~participant
;;

(* Count resting orders per participant across every book, both sides. *)
let resting_counts_by_participant books =
  let counts = Participant.Table.create () in
  List.iter books ~f:(fun (_symbol, book) ->
    List.iter [ Side.Buy; Side.Sell ] ~f:(fun side ->
      List.iter (Order_book.orders_on_side book side) ~f:(fun order ->
        Hashtbl.incr counts (Order.participant order))));
  counts
;;

(* One participant's live resting-order count across every book. *)
let resting_order_count books ~participant =
  Hashtbl.find (resting_counts_by_participant books) participant
  |> Option.value ~default:0
;;

(* [submit_blocked]/[cancel_blocked] answer "should the matching loop reject
   this request?" as [Some reason] (block, [reason] feeds the reject event)
   or [None] (allow). They are pure over the metric windows plus [~books], so
   they unit-test without Async — see [test_metrics.ml]. *)

(* TODO(human): implement the submit-side policy.

   Return [Some reason] to reject [participant]'s next submit, or [None] to
   allow it. Block when EITHER limit is exceeded:
   - resting orders: [resting_order_count books ~participant] against
     [limits.max_resting_orders]; and
   - submit rate: [order_rate t ~now ~participant] (submissions per second in
     the trailing window) against [limits.max_submits_per_sec]. Decide the
     boundary (is exactly-at-the-limit allowed or blocked?) and craft a
     human-readable [reason] for whichever limit tripped. [cancel_blocked]
     below is the worked pattern for the rate half. *)
let submit_blocked t ~now ~participant ~books ~(limits : Rate_limits.t) =
  let rate = submit_rate t ~now ~participant in
  let resting_orders = resting_order_count books ~participant in
  let over_resting = resting_orders > limits.max_resting_orders in
  let over_rate = Float.O.(rate > Float.of_int limits.max_submits_per_sec) in
  match over_resting, over_rate with
  | true, _ ->
    Some
      [%string
        "resting order limit exceeded: %{resting_orders#Int} > \
         %{limits.max_resting_orders#Int}"]
  | _, true ->
    Some
      [%string
        "order rate limit exceeded: %{rate#Float}/s > \
         %{limits.max_submits_per_sec#Int}/s"]
  | false, false -> None
;;

let cancel_blocked t ~now ~participant ~(limits : Rate_limits.t) =
  let rate = cancel_rate t ~now ~participant in
  match Float.( > ) rate (Float.of_int limits.max_cancels_per_sec) with
  | false -> None
  | true ->
    Some
      [%string
        "cancel rate limit exceeded: %{rate#Float}/s > \
         %{limits.max_cancels_per_sec#Int}/s"]
;;

let participant_stats t ~now ~books =
  let resting = resting_counts_by_participant books in
  (* A participant appears if they are either currently submitting or have
     resting interest; otherwise they are dropped to keep the panel focused
     on active accounts. *)
  let participants =
    Participant.Set.union_list
      [ Hashtbl.keys t.order_events |> Participant.Set.of_list
      ; Hashtbl.keys resting |> Participant.Set.of_list
      ]
  in
  Set.to_list participants
  |> List.filter_map ~f:(fun participant ->
    let orders_per_sec = submit_rate t ~now ~participant in
    let resting_order_count =
      Hashtbl.find resting participant |> Option.value ~default:0
    in
    match Float.equal orders_per_sec 0. && resting_order_count = 0 with
    | true -> None
    | false ->
      Some
        { Dashboard_snapshot.Participant_stats.participant
        ; orders_per_sec
        ; resting_order_count
        })
;;

let book_depth ~books ~focus_symbol =
  match List.Assoc.find books ~equal:Symbol.equal focus_symbol with
  | None ->
    { Dashboard_snapshot.Book_depth.symbol = focus_symbol
    ; bbo = Bbo.empty
    ; total_resting_bid_size = Size.zero
    ; total_resting_ask_size = Size.zero
    }
  | Some book ->
    let total_resting_size side =
      Order_book.orders_on_side book side
      |> List.fold ~init:Size.zero ~f:(fun acc order ->
        Size.( + ) acc (Order.remaining_size order))
    in
    { Dashboard_snapshot.Book_depth.symbol = focus_symbol
    ; bbo = Order_book.best_bid_offer book
    ; total_resting_bid_size = total_resting_size Side.Buy
    ; total_resting_ask_size = total_resting_size Side.Sell
    }
;;

let build_snapshot t ~now ~memory ~books ~focus_symbol =
  { Dashboard_snapshot.sampled_at = now
  ; memory
  ; submit_latency = Latency_tracker.submit_stats t.latency ~now
  ; cancel_latency = Latency_tracker.cancel_stats t.latency ~now
  ; participants = participant_stats t ~now ~books
  ; book_depth = book_depth ~books ~focus_symbol
  }
;;

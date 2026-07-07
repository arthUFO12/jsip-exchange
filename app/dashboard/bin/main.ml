open! Core
open! Async_kernel
open Async_kernel.Deferred.Let_syntax
open Jsip_types
open Jsip_protocol
open Jsip_dashboard_web
module Rpc = Async_rpc_kernel.Rpc

(* The symbol whose book depth the dashboard shows: the monitor-feed RPC
   takes it as its query. Hard-coded for now; a later iteration can read it
   from the page URL. *)
let default_focus_symbol = Symbol.of_string "AAPL"

(* Subscribe to the monitor feed and hand back the snapshot pipe. *)
let subscribe conn ~symbol =
  let%map result =
    Rpc.Pipe_rpc.dispatch Rpc_protocol.monitor_feed_rpc conn symbol
  in
  match result with
  | Ok (Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t))) -> pipe
  | Ok (Error err) | Error err ->
    raise_s [%message "monitor-feed dispatch failed" (err : Error.t)]
;;

(* Connect to the exchange over a websocket and forward its snapshot stream
   into [writer]. Runs in the background so the UI renders immediately — the
   dashboard shows "waiting for first snapshot" with or without a live
   server, and starts updating once snapshots arrive. *)
let feed_from_server writer =
  let%bind conn = Async_js.Rpc.Connection.client () >>| Or_error.ok_exn in
  let%bind pipe = subscribe conn ~symbol:default_focus_symbol in
  Pipe.transfer_id pipe writer
;;

let () =
  Async_js.init ();
  let snapshots, writer = Pipe.create () in
  (* Start the UI first, feed it second: never block rendering on the
     network. *)
  Bonsai_web.Start.start (fun (local_ graph) ->
    View.component ~snapshots graph);
  don't_wait_for (feed_from_server writer)
;;

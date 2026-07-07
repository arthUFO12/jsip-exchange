open! Core
open! Async
open Jsip_types
open Jsip_gateway

(* Throwaway diagnostic: connect to the exchange's websocket RPC endpoint the
   same way the browser does, dispatch [monitor_feed_rpc], and print the
   first couple of snapshots. Confirms the server-side websocket path
   independently of the js_of_ocaml frontend. *)
let main ~uri ~symbol =
  match%bind Rpc_websocket.Rpc.client (Uri.of_string uri) with
  | Error e ->
    print_s [%message "websocket connect failed" (e : Error.t)];
    return ()
  | Ok conn ->
    (match%bind
       Rpc.Pipe_rpc.dispatch Rpc_protocol.monitor_feed_rpc conn symbol
     with
     | Error e | Ok (Error e) ->
       print_s [%message "dispatch failed" (e : Error.t)];
       return ()
     | Ok (Ok (pipe, _metadata)) ->
       let seen = ref 0 in
       Pipe.iter pipe ~f:(fun (snapshot : Dashboard_snapshot.t) ->
         incr seen;
         print_s [%sexp (snapshot : Dashboard_snapshot.t)];
         if !seen >= 2 then Pipe.close_read pipe;
         return ()))
;;

let () =
  Command.async
    ~summary:"probe the monitor feed over websocket"
    (let%map_open.Command uri =
       flag
         "-uri"
         (optional_with_default "ws://localhost:8080/" string)
         ~doc:"URI websocket endpoint"
     in
     fun () -> main ~uri ~symbol:(Symbol.of_string "AAPL"))
  |> Command_unix.run
;;

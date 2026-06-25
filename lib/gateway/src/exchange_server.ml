open! Core
open! Async
open Jsip_types
open Jsip_order_book

module type Connection_state_sig = sig
  type t = { mutable session : Session.t option }

  val _participant : t -> Participant.t option
end

module Connection_state : Connection_state_sig = struct
  type t = { mutable session : Session.t option }

  let _participant t = Option.map t.session ~f:Session.participant
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Order.Request.t Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_submit
  ~request_writer
  (request : Order.Request.t)
  (state : Connection_state.t)
  =
  match state.session with
  | None -> return (Or_error.error_string "not logged in")
  | Some session ->
    let sanitized_request =
      { request with participant = Session.participant session }
    in
    let%map () = Pipe.write_if_open request_writer sanitized_request in
    Ok ()
;;

let start_matching_loop ~engine ~dispatcher request_reader =
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       let events = Matching_engine.submit engine request in
       Dispatcher.dispatch dispatcher events))
;;

let on_close_hook conn dispatcher session =
  match session with
  | None -> ()
  | Some active_session ->
    don't_wait_for
      (Async.Deferred.bind (Rpc.Connection.close_finished conn) ~f:(fun () ->
         Dispatcher.clean_up_session dispatcher active_session))
;;

let handle_login dispatcher name =
  match String.strip name with
  | "" -> return (Or_error.error_string "empty string")
  | s ->
    let participant = Participant.of_string s in
    let%bind () = Dispatcher.set_up_session dispatcher participant in
    return (Ok participant)
;;

let handle_session_feed (state : Connection_state.t) =
  match state.session with
  | None -> Deferred.Or_error.error_string "not logged in"
  | Some session -> Deferred.Or_error.return (Session.reader session)
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher request_reader;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               handle_submit ~request_writer request state)
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun (state : Connection_state.t) () ->
               handle_session_feed state)
        ; Rpc.Rpc.implement
            Rpc_protocol.login_rpc
            (fun (state : Connection_state.t) (name : string) ->
               let%bind participant_or_error =
                 handle_login dispatcher name
               in
               (match participant_or_error with
                | Ok participant ->
                  state.session
                  <- Some (Dispatcher.get_session_exn dispatcher participant)
                | Error _ -> ());
               return participant_or_error)
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr conn ->
        let initial_connection_state =
          ({ session = None } : Connection_state.t)
        in
        on_close_hook conn dispatcher initial_connection_state.session;
        initial_connection_state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server

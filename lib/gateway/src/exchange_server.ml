open! Core
open! Async
open Jsip_types
open Jsip_order_book

module type Connection_state_sig = sig
  type t = { mutable session : Session.t option }
end

module Connection_state : Connection_state_sig = struct
  type t = { mutable session : Session.t option }
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

let handle_submit ~request_writer ~dispatcher (request : Order.Request.t) =
  match Dispatcher.get_session dispatcher request.participant with
  | None -> Deferred.Or_error.error_string "not logged in"
  | Some session ->
    let sanitized_request =
      { request with participant = Session.participant session }
    in
    (match
       Dispatcher.register_coid_to_participant
         dispatcher
         ~participant:sanitized_request.participant
         ~client_order_id:sanitized_request.client_order_id
     with
     | false ->
       Dispatcher.dispatch
         dispatcher
         [ Exchange_event.Order_reject
             { request = sanitized_request
             ; reason = "duplicate client order id"
             }
         ];
       Deferred.Or_error.error_string "duplicate client order id"
     | true ->
       let%map () = Pipe.write_if_open request_writer sanitized_request in
       Ok ())
;;

let handle_cancel ~engine ~dispatcher session client_order_id =
  let events_list = ref [] in
  let result = ref (Or_error.return ()) in
  match Option.map session ~f:Session.participant with
  | None ->
    events_list
    := Exchange_event.Cancel_reject
         { participant = Participant.of_string ""
         ; client_order_id
         ; reason = "not logged in"
         }
       :: !events_list;
    result := Or_error.error_string "not logged in"
  | Some participant ->
    (match Dispatcher.get_order dispatcher ~participant ~client_order_id with
     | None ->
       events_list
       := Exchange_event.Cancel_reject
            { participant; client_order_id; reason = "not found" }
          :: !events_list;
       result := Or_error.error_string "not found"
     | Some order ->
       (match Matching_engine.cancel engine order with
        | Error e ->
          events_list
          := Exchange_event.Cancel_reject
               { participant
               ; client_order_id
               ; reason = Error.to_string_hum e
               }
             :: !events_list;
          result := Or_error.error_string (Error.to_string_hum e)
        | Ok is_best_price ->
          if is_best_price
          then (
            let order_symbol = Order.symbol order in
            events_list
            := Exchange_event.Best_bid_offer_update
                 { symbol = order_symbol
                 ; bbo =
                     Matching_engine.book engine order_symbol
                     |> Option.value_exn
                     |> Order_book.best_bid_offer
                 }
               :: !events_list);
          events_list
          := Exchange_event.Order_cancel
               { order_id = Order.order_id order
               ; participant = Order.participant order
               ; symbol = Order.symbol order
               ; remaining_size = Order.remaining_size order
               ; reason = Cancel_reason.of_string "PARTICIPANT_REQUESTED"
               ; client_order_id
               }
             :: !events_list;
          result := Ok ()));
    Dispatcher.dispatch dispatcher !events_list
;;

let start_matching_loop ~engine ~dispatcher request_reader =
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       let order, events = Matching_engine.submit engine request in
       match order with
       | None -> ()
       | Some order ->
         Dispatcher.register_order_to_coid_participant_pair
           dispatcher
           ~participant:request.participant
           ~client_order_id:request.client_order_id
           ~order;
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
            (fun _state request ->
               handle_submit ~request_writer ~dispatcher request)
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
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun
                (state : Connection_state.t)
                (client_order_id : Client_order_id.t)
              ->
               Deferred.Or_error.return
                 (handle_cancel
                    ~engine
                    ~dispatcher
                    state.session
                    client_order_id))
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

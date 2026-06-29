open! Core
open! Async
open Jsip_types

module Session_data = struct
  type t =
    { session : Session.t
    ; coid_to_orders : (Order.t option) Client_order_id.Table.t
    }
end

type t =
  { market_data_subscribers_by_symbol :
      Exchange_event.t Pipe.Writer.t Bag.t Symbol.Table.t
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; sessions_data : Session_data.t Participant.Table.t
  }

let create () =
  { market_data_subscribers_by_symbol = Symbol.Table.create ()
  ; audit_subscribers = Bag.create ()
  ; sessions_data = Participant.Table.create ()
  }
;;

let clean_up_session t session =
  Hashtbl.remove t.sessions_data (Session.participant session);
  Session.close session
;;

let set_up_session t participant =
  let new_session = Session.create participant in
  let%bind () =
    match Hashtbl.find t.sessions_data participant with
    | None -> return ()
    | Some old_session_data -> clean_up_session t old_session_data.session
  in
  ignore
    (Hashtbl.add
       t.sessions_data
       ~key:participant
       ~data:
         { session = new_session
         ; coid_to_orders = Client_order_id.Table.create ()
         });
  return ()
;;

let get_session_exn t participant =
  (Hashtbl.find_exn t.sessions_data participant).session
;;

let get_session t participant =
  Hashtbl.find t.sessions_data participant
  |> Option.map ~f:(fun session_data -> session_data.session)
;;

let subscribe_market_data t symbols =
  let reader, writer = Pipe.create () in
  (* Register the same writer in every requested symbol's bag. A per-symbol
     publish iterates a single bag, so a subscriber listed in multiple bags
     receives each event exactly once — only via whichever bag matches the
     event's symbol. *)
  let elts =
    List.map symbols ~f:(fun symbol ->
      let subscribers =
        Hashtbl.find_or_add
          t.market_data_subscribers_by_symbol
          ~default:Bag.create
          symbol
      in
      symbol, Bag.add subscribers writer)
  in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     List.iter elts ~f:(fun (symbol, elt) ->
       match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
       | None -> ()
       | Some subscribers -> Bag.remove subscribers elt));
  reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

let push_market_data t event symbol =
  match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
  | None -> ()
  | Some subscribers ->
    Bag.iter subscribers ~f:(fun writer ->
      Pipe.write_without_pushback_if_open writer event)
;;

let push_audit t event =
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer event)
;;

let push_to_session t participant event =
  (* TODO: Once sessions have been implemented this function should write the
     event to the appropriate session's pipe. For now we have the server
     binary print these events to stdout while tests can silence them. *)
  match Hashtbl.find t.sessions_data participant with 
    | None -> ()
    | Some session_data -> Session.push session_data.session event;
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; request }
  | Order_reject { request; reason = _ } ->
    push_to_session t request.participant event
  | Order_cancel
      { order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      ; client_order_id = _
      } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_client_order_id = _
      ; aggressor_side = _
      ; resting_order_id = _
      ; resting_participant
      ; resting_client_order_id = _
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
    | Cancel_reject {
      participant
      ; client_order_id = _
      ; reason = _
    } ->
      push_to_session t participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end

let register_coid_to_participant t ~participant ~client_order_id =
  let session_data = Hashtbl.find_exn t.sessions_data participant in
  match Hashtbl.mem session_data.coid_to_orders client_order_id with
  | true -> false
  | false ->
    ignore (Hashtbl.add session_data.coid_to_orders ~key:client_order_id ~data:None);
    true
;;

let register_order_to_coid_participant_pair t ~participant ~client_order_id ~order =
  let sessions_data = (Hashtbl.find_exn t.sessions_data participant) in 
  Hashtbl.update sessions_data.coid_to_orders client_order_id ~f:(function 
  | None -> None 
  | Some _ -> Some order)

let get_order t ~participant ~client_order_id =
  let session_data = Hashtbl.find_exn t.sessions_data participant in
  Hashtbl.find session_data.coid_to_orders client_order_id |> Option.join
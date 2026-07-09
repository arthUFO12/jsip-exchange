open! Core

type t =
  { side : Side.t
  ; mutable price_levels : (Order_id.t, Order.t) My_hash_queue.t Price.Map.t
  ; id_to_price : (Order_id.t, Price.t) Hashtbl_intf.Hashtbl.t
  ; mutable best_price : Price.t option
  }
[@@deriving sexp_of]

let add t order =
  let order_price = Order.price order in
  let order_id = Order.order_id order in
  let order_queue =
    match Map.find t.price_levels order_price with
    | None ->
      let new_queue = My_hash_queue.create (module Order_id) in
      t.price_levels
      <- Map.set t.price_levels ~key:order_price ~data:new_queue;
      (match t.best_price with
       | Some bp
         when Price.is_more_aggressive t.side ~price:order_price ~than:bp ->
         t.best_price <- Some order_price
       | Some _ -> ()
       | None -> t.best_price <- Some order_price);
      new_queue
    | Some queue -> queue
  in
  Hashtbl.add_exn t.id_to_price ~key:order_id ~data:order_price;
  ignore (My_hash_queue.enqueue order_queue order_id order)
;;

let clean_up_price_level t (price : Price.t) =
  let price_queue = Map.find_exn t.price_levels price in
  if My_hash_queue.is_empty price_queue
  then (
    t.price_levels <- Map.remove t.price_levels price;
    match t.best_price with
    | Some bp when Price.( = ) price bp ->
      let best_price_tuple =
        match (t.side : Side.t) with
        | Buy -> Map.max_elt t.price_levels
        | Sell -> Map.min_elt t.price_levels
      in
      let new_best_price = Option.map best_price_tuple ~f:fst in
      t.best_price <- new_best_price
    | _ -> ())
;;

let remove t order_id =
  match Hashtbl.find t.id_to_price order_id with
  | None -> None
  | Some order_price ->
    let price_queue = Map.find_exn t.price_levels order_price in
    let order = My_hash_queue.remove price_queue order_id in
    Hashtbl.remove t.id_to_price order_id;
    clean_up_price_level t order_price;
    order
;;

let create side =
  let map = Price.Map.empty in
  let id_to_price = Hashtbl.create ~growth_allowed:true (module Order_id) in
  { side; price_levels = map; id_to_price; best_price = None }
;;

let best_price t = t.best_price

let list_out t =
  let all_orders = Queue.create () in
  Map.iter t.price_levels ~f:(fun q ->
    My_hash_queue.iter q ~f:(fun order -> Queue.enqueue all_orders order));
  match (t.side : Side.t) with
  | Buy -> Queue.to_list all_orders |> List.rev
  | Sell -> Queue.to_list all_orders
;;

let is_empty t = Map.is_empty t.price_levels

let size t =
  Map.fold t.price_levels ~init:0 ~f:(fun ~key:_ ~data sum ->
    sum + My_hash_queue.length data)
;;

let find t order_id =
  match Hashtbl.find t.id_to_price order_id with
  | None -> None
  | Some order_price ->
    let price_queue = Map.find_exn t.price_levels order_price in
    My_hash_queue.find price_queue order_id
;;

let find_best_price_time_match t price buy_or_sell =
  match (buy_or_sell : Side.t) with
  | Buy ->
    (match Map.min_elt t.price_levels with
     | None -> None
     | Some (key, queue) when Price.( <= ) key price -> My_hash_queue.peek queue
     | Some _ -> None)
  | Sell ->
    (match Map.max_elt t.price_levels with
     | None -> None
     | Some (key, queue) when Price.( >= ) key price -> My_hash_queue.peek queue
     | Some _ -> None)
;;

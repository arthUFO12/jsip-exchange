open! Core


type t =
  { side : Side.t
  ; mutable price_levels : Order.t Queue.t Price.Map.t
  ; id_to_price : (Order_id.t, Price.t) Hashtbl_intf.Hashtbl.t
  }
[@@deriving sexp_of]

let add t order =
  let order_price = Order.price order in
  let order_id = Order.order_id order in
  let order_queue =
    match Map.find t.price_levels order_price with
    | None ->
      t.price_levels
      <- (match
            Map.add t.price_levels ~key:order_price ~data:(Queue.create ())
          with
          | `Duplicate -> t.price_levels
          | `Ok map -> map);
      Map.find_exn t.price_levels order_price
    | Some queue -> queue
  in
  Hashtbl.add_exn t.id_to_price ~key:order_id ~data:order_price;
  Queue.enqueue order_queue order
;;

let clean_up_price_level t (price : Price.t) =
  let price_queue = Map.find_exn t.price_levels price in
  if Queue.is_empty price_queue
  then t.price_levels <- Map.remove t.price_levels price
;;

let remove t order_id =
  match Hashtbl.find t.id_to_price order_id with
  | None -> None
  | Some order_price ->
    let price_queue = Map.find_exn t.price_levels order_price in
    let front_id = Queue.peek_exn price_queue |> Order.order_id in
    let order =
      if Order_id.equal order_id front_id
      then Queue.dequeue price_queue
      else (
        let equal_func x = Order_id.equal (Order.order_id x) order_id in
        let found_order = Queue.find price_queue ~f:equal_func in
        Queue.filter_inplace price_queue ~f:(fun ele ->
          not (equal_func ele));
        found_order)
    in
    Hashtbl.remove t.id_to_price order_id;
    clean_up_price_level t order_price;
    order
;;

let create side =
  let map = Price.Map.empty in
  let id_to_price =
    Hashtbl.create ~growth_allowed:true (module Order_id)
  in
  { side; price_levels = map; id_to_price }
;;

let best_price t =
  let best_price =
    match (t.side : Side.t) with
    | Buy -> Map.max_elt t.price_levels
    | Sell -> Map.min_elt t.price_levels
  in
  match best_price with None -> None | Some (price, _) -> Some price
;;

let list_out t =
  let list = ref [] in
  Map.iter t.price_levels ~f:(fun q ->
    list
    := match (t.side : Side.t) with
        | Buy -> Queue.to_list q @ !list
        | Sell -> !list @ Queue.to_list q);
  !list
;;

let is_empty t = Map.is_empty t.price_levels

let size t =
  Map.fold t.price_levels ~init:0 ~f:(fun ~key:_ ~data sum ->
    sum + Queue.length data)
;;

let find t order_id =
  match Hashtbl.find t.id_to_price order_id with
  | None -> None
  | Some order_price ->
    let price_queue = Map.find_exn t.price_levels order_price in
    Queue.find price_queue ~f:(fun x ->
      Order_id.equal (Order.order_id x) order_id)
;;

let find_best_price_time_match t price buy_or_sell =
  match (buy_or_sell : Side.t) with
  | Buy ->
    (match Map.min_elt t.price_levels with
      | None -> None
      | Some (key, queue) when Price.( <= ) key price -> Queue.peek queue
      | Some _ -> None)
  | Sell ->
    (match Map.max_elt t.price_levels with
      | None -> None
      | Some (key, queue) when Price.( >= ) key price -> Queue.peek queue
      | Some _ -> None)
;;

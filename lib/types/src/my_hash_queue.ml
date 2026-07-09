open! Core

module Queue_node = struct
  type ('k, 'v) t =
    { mutable next : ('k, 'v) t option
    ; mutable last : ('k, 'v) t option
    ; kv_pair : 'k * 'v
    }

  let create k v = { next = None; last = None; kv_pair = k, v }
end

module Internal_queue = struct
  type ('k, 'v) t =
    { mutable head : ('k, 'v) Queue_node.t option
    ; mutable tail : ('k, 'v) Queue_node.t option
    ; mutable size : int
    }

  let create () = { head = None; tail = None; size = 0 }

  let enqueue t k v =
    let new_node = Queue_node.create k v in
    (match t.tail with
     | None ->
       t.head <- Some new_node;
       t.tail <- Some new_node
     | Some node ->
       node.next <- Some new_node;
       new_node.last <- Some node;
       t.tail <- Some new_node);
    t.size <- t.size + 1
  ;;

  let dequeue t =
    match t.head with
    | None -> None
    | Some node ->
      t.head <- node.next;
      (match t.head with
       | Some head -> head.last <- None
       | None -> t.tail <- None);
      t.size <- t.size - 1;
      Some node.kv_pair
  ;;

  let splice_out t (node : ('k, 'v) Queue_node.t) =
    (match node.last, node.next with
     | None, None ->
       t.head <- None;
       t.tail <- None
     | None, Some node ->
       t.head <- Some node;
       node.last <- None
     | Some node, None ->
       t.tail <- Some node;
       node.next <- None
     | Some first_node, Some second_node ->
       first_node.next <- Some second_node;
       second_node.last <- Some first_node);
    t.size <- t.size - 1
  ;;

  let iter t ~f =
    let rec iter_helper (node_option : ('k, 'v) Queue_node.t option) =
      match node_option with
      | None -> ()
      | Some node ->
        f (snd node.kv_pair);
        iter_helper node.next
    in
    iter_helper t.head
  ;;

end

type ('k, 'v) t =
  { queue : ('k, 'v) Internal_queue.t
  ; table : ('k, ('k, 'v) Queue_node.t) Hashtbl.t
  }

let create key =
  { queue = Internal_queue.create (); table = Hashtbl.create key }
;;

let enqueue t k v =
  match Hashtbl.find t.table k with
  | None ->
    Internal_queue.enqueue t.queue k v;
    Hashtbl.set t.table ~key:k ~data:(Option.value_exn t.queue.tail);
    true
  | Some _ -> false
;;

let dequeue t =
  match Internal_queue.dequeue t.queue with
  | Some (key, value) ->
    Hashtbl.remove t.table key;
    Some value
  | None -> None
;;

let remove t k =
  match Hashtbl.find_and_remove t.table k with
  | None -> None
  | Some queue_node ->
    Internal_queue.splice_out t.queue queue_node;
    Some (snd queue_node.kv_pair)
;;

let find t k =
  Hashtbl.find t.table k |> Option.map ~f:(fun node -> snd node.kv_pair)
;;

let peek t = Option.map t.queue.head ~f:(fun node -> snd node.kv_pair)
let length t = t.queue.size
let iter t ~f = Internal_queue.iter t.queue ~f
let is_empty t = t.queue.size = 0
let sexp_of_t _sexp_of_k _sexp_of_v _t = Sexp.Atom "_"

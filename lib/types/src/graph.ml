open! Core

module Node = struct
  type t =
    { id : Symbol.t
    ; out_neighbors : t Symbol.Table.t
    }

  let create symbol = { id = symbol; out_neighbors = Symbol.Table.create () }
end

type t = { nodes : Node.t Symbol.Table.t }

let create () = { nodes = Symbol.Table.create () }

let add_node t symbol =
  Hashtbl.set t.nodes ~key:symbol ~data:(Node.create symbol)
;;

let add_edge t first second =
  let first_node = Hashtbl.find_exn t.nodes first in
  let second_node = Hashtbl.find_exn t.nodes second in
  Hashtbl.set first_node.out_neighbors ~key:second ~data:second_node;
  Hashtbl.set second_node.out_neighbors ~key:first ~data:first_node
;;

let bfs t start =
  let to_visit = Queue.create () in
  let visited = Hash_set.create (module Symbol) in
  let rec bfs_helper () =
    if Queue.is_empty to_visit
    then ()
    else (
      let curr_node =
        Queue.dequeue to_visit
        |> Option.value_exn
        |> Hashtbl.find_exn t.nodes
      in
      Hashtbl.iteri
        curr_node.out_neighbors
        ~f:(fun ~key:_ ~data:(neighbor : Node.t) ->
          if not (Hash_set.mem visited neighbor.id)
          then (
            Queue.enqueue to_visit neighbor.id;
            Hash_set.add visited neighbor.id));
      bfs_helper ())
  in
  Queue.enqueue to_visit start;
  Hash_set.add visited start;
  bfs_helper ();
  visited
;;

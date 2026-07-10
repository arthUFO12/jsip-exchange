(** A FIFO queue with O(1) removal by key.

    Backed by a doubly-linked list (which gives FIFO order) plus a
    {!Core.Hashtbl} mapping each key to its list node (which gives
    constant-time lookup). Unlike a plain queue, an element can be pulled out
    of the middle in O(1) given its key — see {!remove}. Keys are unique:
    {!enqueue} rejects a key that is already present.

    Used to track resting orders where cancellation-by-id must be cheap. *)

open! Core

type ('k, 'v) t

(** [create m] is an empty queue keyed by the module [m], e.g.
    [create (module String)]. [m] supplies hashing and equality for the keys. *)
val create : 'k Base.Hashtbl.Key.t -> ('k, 'v) t

(** [enqueue t k v] appends [v] at the back of [t] under key [k]. Returns
    [false] and does nothing if [k] is already present; [true] otherwise. *)
val enqueue : ('k, 'v) t -> 'k -> 'v -> bool

(** [dequeue t] removes and returns the value at the front of [t] (the least
    recently enqueued), or [None] if [t] is empty. *)
val dequeue : ('k, 'v) t -> 'v option

(** [remove t k] removes [k]'s entry from anywhere in [t] and returns its
    value, or [None] if [k] is absent. *)
val remove : ('k, 'v) t -> 'k -> 'v option

val find : ('k, 'v) t -> 'k -> 'v option

(** [length t] is the number of elements currently in [t]. *)
val length : ('k, 'v) t -> int

val is_empty : ('k, 'v) t -> bool
val peek : ('k, 'v) t -> 'v option
val iter : ('k, 'v) t -> f:('v -> unit) -> unit

val sexp_of_t
  :  ('k -> Sexp.t)
  -> ('v -> Sexp.t)
  -> ('k, 'v) t
  -> Sexp_type.Sexp.t

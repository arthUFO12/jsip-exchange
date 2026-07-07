(** Bonsai_web rendering for the dashboard.

    Wraps the pure {!Controller} state machine into a bonsai_web component,
    mirroring [app/monitor/src/term_app.mli] for the terminal monitor. The
    controller's state lives in a [Bonsai.state_machine] inside [component];
    on graph activation [component] drains the supplied [snapshots] pipe into
    that machine, and it wires both tab clicks and number-key presses to
    {!Controller.select}. *)

open! Core
open! Async_kernel
open Jsip_protocol
open Jsip_dashboard
open! Bonsai_web

(** The dashboard component. Pass its result to [Bonsai_web.Start.start].
    [snapshots] is the pipe of {!Dashboard_snapshot.t}s to render (typically
    the reader from [Rpc_protocol.monitor_feed_rpc]); the drain starts when
    the graph activates and ends when [snapshots] closes. *)
val component
  :  snapshots:Dashboard_snapshot.t Pipe.Reader.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t

module For_testing : sig
  (** Pure renderer from a {!Controller.Display.t} to a virtual-dom node.
      [select] is the effect fired by a tab click or number key. *)
  val render
    :  Controller.Display.t
    -> select:(Controller.Focus.t -> unit Effect.t)
    -> Vdom.Node.t
end

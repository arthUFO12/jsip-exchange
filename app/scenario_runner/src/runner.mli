(** Glue that boots a scenario into a running exchange + ecosystem of bots. *)

open! Core
open! Async

(** Boot the exchange on [port], spin up the oracle/news/bots described by
    [config], and return a deferred that resolves only when the server is
    closed. The deferred for each bot's tick loop is leaked via
    [don't_wait_for].

    When [dashboard] is [true], also serve the web dashboard (WebSocket RPC +
    static files) on [http_port], out of [dashboard_dir] (the directory
    holding [index.html] and the compiled [main.bc.js]) — the browser
    dashboard then watches the very same exchange the scenario's bots are
    trading on. *)
val run
  :  Scenario_config.t
  -> port:int
  -> seed:int
  -> dashboard:bool
  -> http_port:int
  -> dashboard_dir:string
  -> unit Deferred.t

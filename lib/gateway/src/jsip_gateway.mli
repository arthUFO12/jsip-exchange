(** Gateway layer for the JSIP exchange.

    Provides RPC definitions for client-server communication, the exchange
    server that bundles the matching engine with network handling, and the
    [Dispatcher] that routes matching-engine events to the right subscribers
    (per-participant session feeds, per-symbol market data, audit firehose).

    The monitoring stack — [Dashboard_snapshot] (the streamed record),
    [Latency_tracker] (submit/cancel timing), and [Metrics] (the aggregator
    that assembles snapshots) — feeds the operator dashboard in [app/monitor]
    over {!Rpc_protocol.monitor_feed_rpc}. *)

module Protocol = Protocol
module Rpc_protocol = Jsip_protocol.Rpc_protocol
module Session = Session
module Dispatcher = Dispatcher
module Exchange_server = Exchange_server
module Exchange_command = Exchange_command
module Dashboard_snapshot = Jsip_protocol.Dashboard_snapshot
module Latency_tracker = Latency_tracker
module Metrics = Metrics
module Rate_limits = Rate_limits

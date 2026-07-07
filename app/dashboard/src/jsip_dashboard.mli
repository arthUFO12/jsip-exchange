(** Pure state machine for the JSIP exchange web dashboard.

    [Controller] consumes the {!Jsip_gateway.Rpc_protocol.monitor_feed_rpc}
    snapshot stream into a rolling one-minute window plus a selected pane,
    and projects it into a plain-data [Controller.Display.t]. It is
    deliberately free of any bonsai_web dependency so it stays unit-testable
    as ordinary data; the bonsai_web rendering lives in the separate
    [jsip_dashboard_web] library ([View]), which cannot be linked into a
    native inline-test runner. *)

module Controller = Controller

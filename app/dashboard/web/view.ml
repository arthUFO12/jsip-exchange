open! Core
open! Async_kernel
open Jsip_types
open Jsip_protocol
open Jsip_dashboard
open! Bonsai_web

(* Actions folded into the controller's state machine. [Feed_snapshot]
   arrives from the monitor-feed pipe we drain on activation; [Select] comes
   from a tab click or a number-key press. *)
module Action = struct
  type t =
    | Feed_snapshot of Dashboard_snapshot.t
    | Select of Controller.Focus.t
  [@@deriving sexp_of]
end

(* ---------- pure rendering (Display.t -> Vdom) ---------- *)

(* ---------- live-words line graph (SVG) ---------- *)

(* A fixed viewBox coordinate space the browser scales to the pane's width;
   the height then follows by aspect ratio, which is what gives the pane real
   vertical size. Margins leave room for the y-axis ticks and title. *)
let graph_w = 260.
let graph_h = 120.
let plot_left = 46.
let plot_right = graph_w -. 8.
let plot_top = 10.
let plot_bottom = graph_h -. 22.
let plot_mid_y = (plot_top +. plot_bottom) /. 2.
let svg tag attrs children = Vdom.Node.create_svg tag ~attrs children
let svg_attr = Vdom.Attr.create
let coord f = sprintf "%.1f" f

(* Compact axis label: 679646 -> "679k". *)
let abbrev v =
  if v >= 1000 then sprintf "%dk" (v / 1000) else Int.to_string v
;;

(* Line graph of live_words across the rolling window. The y-axis is labeled
   with its title and the current min/max of the window; a flat series (or a
   window with a single sample) is handled without dividing by zero. *)
let live_words_graph (values : int list) =
  match values with
  | [] | [ _ ] ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "graph-empty" ]
      [ Vdom.Node.text "collecting…" ]
  | _ :: _ :: _ ->
    let n = List.length values in
    let lo = List.min_elt values ~compare:Int.compare |> Option.value_exn in
    let hi = List.max_elt values ~compare:Int.compare |> Option.value_exn in
    let x_of i =
      plot_left
      +. ((plot_right -. plot_left) *. Float.of_int i /. Float.of_int (n - 1))
    in
    let y_of v =
      match Int.equal hi lo with
      | true -> plot_mid_y
      | false ->
        plot_bottom
        -. (Float.of_int (v - lo)
            /. Float.of_int (hi - lo)
            *. (plot_bottom -. plot_top))
    in
    let points =
      List.mapi values ~f:(fun i v ->
        sprintf "%s,%s" (coord (x_of i)) (coord (y_of v)))
      |> String.concat ~sep:" "
    in
    let axis x1 y1 x2 y2 =
      svg
        "line"
        [ svg_attr "x1" (coord x1)
        ; svg_attr "y1" (coord y1)
        ; svg_attr "x2" (coord x2)
        ; svg_attr "y2" (coord y2)
        ; Vdom.Attr.class_ "axis"
        ]
        []
    in
    let tick ~y ~label =
      svg
        "text"
        [ svg_attr "x" (coord (plot_left -. 4.))
        ; svg_attr "y" (coord y)
        ; Vdom.Attr.class_ "tick"
        ]
        [ Vdom.Node.text label ]
    in
    svg
      "svg"
      [ svg_attr "viewBox" (sprintf "0 0 %.0f %.0f" graph_w graph_h)
      ; Vdom.Attr.class_ "trend"
      ]
      [ axis plot_left plot_top plot_left plot_bottom
      ; axis plot_left plot_bottom plot_right plot_bottom
      ; svg
          "polyline"
          [ svg_attr "points" points; Vdom.Attr.class_ "line" ]
          []
      ; tick ~y:(plot_top +. 4.) ~label:(abbrev hi)
      ; tick ~y:plot_bottom ~label:(abbrev lo)
      ; svg
          "text"
          [ svg_attr "x" "10"
          ; svg_attr "y" (coord plot_mid_y)
          ; svg_attr
              "transform"
              (sprintf "rotate(-90 10 %s)" (coord plot_mid_y))
          ; Vdom.Attr.class_ "axis-title"
          ]
          [ Vdom.Node.text "live words" ]
      ]
;;

let ms_str x = sprintf "%.2f" x
let rate_str x = sprintf "%.1f" x

let kv label value =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "kv" ]
    [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "k" ] [ Vdom.Node.text label ]
    ; Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "v" ] [ Vdom.Node.text value ]
    ]
;;

let panel ~title ~focused children =
  let classes =
    if focused then [ "panel"; "panel-focused" ] else [ "panel" ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes classes ]
    (Vdom.Node.h2 [ Vdom.Node.text title ] :: children)
;;

let render_memory ~focused (m : Controller.Display.Memory_panel.t) =
  panel
    ~title:"Memory / GC"
    ~focused
    [ kv "live words" (Int.to_string_hum m.live_words)
    ; kv "major words" (Float.to_string_hum m.major_words)
    ; kv "minor words" (Float.to_string_hum m.minor_words)
    ; kv "major collections" (Int.to_string m.major_collections)
    ; kv "minor collections" (Int.to_string m.minor_collections)
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "graph-caption" ]
        [ Vdom.Node.text "live words — last 60s" ]
    ; live_words_graph m.live_words_trend
    ]
;;

let th text = Vdom.Node.th [ Vdom.Node.text text ]
let td text = Vdom.Node.td [ Vdom.Node.text text ]

let latency_tr (r : Controller.Display.Latency_panel.row) =
  Vdom.Node.tr
    [ td r.label
    ; td (Int.to_string r.count)
    ; td (ms_str r.mean_ms)
    ; td (ms_str r.p50_ms)
    ; td (ms_str r.p90_ms)
    ; td (ms_str r.p99_ms)
    ; td (ms_str r.max_ms)
    ]
;;

let render_latency ~focused (l : Controller.Display.Latency_panel.t) =
  panel
    ~title:"Latency (ms)"
    ~focused
    [ Vdom.Node.table
        [ Vdom.Node.tr
            [ th ""
            ; th "n"
            ; th "mean"
            ; th "p50"
            ; th "p90"
            ; th "p99"
            ; th "max"
            ]
        ; latency_tr l.submit
        ; latency_tr l.cancel
        ]
    ]
;;

let render_participants
  ~focused
  (rows : Controller.Display.Participant_row.t list)
  =
  let body =
    match rows with
    | [] -> [ Vdom.Node.text "(no active participants)" ]
    | _ :: _ ->
      [ Vdom.Node.table
          (Vdom.Node.tr [ th "participant"; th "orders/sec"; th "resting" ]
           :: List.map rows ~f:(fun r ->
             Vdom.Node.tr
               [ td r.name
               ; td (rate_str r.orders_per_sec)
               ; td (Int.to_string r.resting_order_count)
               ]))
      ]
  in
  panel ~title:"Participants" ~focused body
;;

let render_book_depth ~focused (b : Controller.Display.Book_depth_panel.t) =
  panel
    ~title:"Book depth"
    ~focused
    [ kv "symbol" (Symbol.to_string b.symbol)
    ; kv "BBO" (Bbo.to_string b.bbo)
    ; kv "total bid size" (Size.to_string b.total_bid_size)
    ; kv "total ask size" (Size.to_string b.total_ask_size)
    ]
;;

let render_tabs ~select (display : Controller.Display.t) =
  let tab ~focused ~label ~target =
    let classes = if focused then [ "tab"; "tab-focused" ] else [ "tab" ] in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.classes classes
        ; Vdom.Attr.on_click (fun _ev -> select target)
        ]
      [ Vdom.Node.text label ]
  in
  let all_tab =
    tab ~focused:display.show_all ~label:"all" ~target:Controller.Focus.All
  in
  let pane_tabs =
    List.map display.tabs ~f:(fun (t : Controller.Display.Tab.t) ->
      tab
        ~focused:t.focused
        ~label:t.label
        ~target:(Controller.Focus.Single t.pane))
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "tabs" ] (all_tab :: pane_tabs)
;;

(* A pane is shown in the [All] overview, or when it is the single focused
   pane. In overview mode nothing is highlighted; in single mode the one
   shown pane is highlighted as being at the forefront. *)
let render_panes
  ~memory
  ~latency
  ~book_depth
  (display : Controller.Display.t)
  =
  List.filter_map Controller.Pane.all ~f:(fun pane ->
    let focused =
      match display.focus with
      | All -> false
      | Single p -> Controller.Pane.equal pane p
    in
    match display.show_all || focused with
    | false -> None
    | true ->
      Some
        (match (pane : Controller.Pane.t) with
         | Memory -> render_memory ~focused memory
         | Latency -> render_latency ~focused latency
         | Participants -> render_participants ~focused display.participants
         | Book_depth -> render_book_depth ~focused book_depth))
;;

(* Number keys bring a pane forward: '0' the overview, '1'..'4' a pane. Read
   the key off the [keyCode] so we avoid the browser-specific [key] string. *)
let focus_of_keycode keycode =
  if keycode >= Char.to_int '0' && keycode <= Char.to_int '9'
  then Controller.Focus.of_hotkey (Char.of_int_exn keycode)
  else None
;;

let on_keydown ~select =
  Vdom.Attr.on_keydown (fun ev ->
    match focus_of_keycode ev##.keyCode with
    | Some focus -> select focus
    | None -> Effect.return ())
;;

let render (display : Controller.Display.t) ~select : Vdom.Node.t =
  let panes_class =
    if display.show_all then "panes panes-all" else "panes panes-single"
  in
  let body =
    match display.memory, display.latency, display.book_depth with
    | Some memory, Some latency, Some book_depth ->
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ panes_class ]
        (render_panes ~memory ~latency ~book_depth display)
    | _ -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "panes" ] []
  in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.id "dashboard"
      ; Vdom.Attr.tabindex 0
      ; Vdom.Attr.autofocus true
      ; on_keydown ~select
      ]
    [ Vdom.Node.h1 [ Vdom.Node.text display.title ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "status" ]
        [ Vdom.Node.text display.status ]
    ; render_tabs ~select display
    ; body
    ]
;;

(* ---------- bonsai wiring ---------- *)

(* On activation, spawn a background task that drains the snapshot pipe
   straight into the state machine, mirroring [app/monitor/src/term_app.ml].
   [expert_handle_as_deferred] schedules each [inject] effect and resolves
   when it has run, so [Pipe.iter] gives the feed natural backpressure. *)
let drain_snapshots_on_activate snapshots inject =
  Effect.of_thunk (fun () ->
    don't_wait_for
      (Pipe.iter snapshots ~f:(fun snapshot ->
         Effect.expert_handle_as_deferred
           (inject (Action.Feed_snapshot snapshot)))))
;;

let component ~snapshots (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~default_model:(Controller.create ())
      ~apply_action:(fun _ctx model action ->
        match (action : Action.t) with
        | Feed_snapshot snapshot -> Controller.feed_snapshot model snapshot
        | Select focus -> Controller.select model focus)
      graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%map.Bonsai inject in
       drain_snapshots_on_activate snapshots inject)
    graph;
  let display = Bonsai.map model ~f:Controller.display in
  let%map.Bonsai display and inject in
  render display ~select:(fun focus -> inject (Action.Select focus))
;;

module For_testing = struct
  let render = render
end

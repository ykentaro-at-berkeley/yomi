(* c872be81-c6bc-5fa9-bb33-4e9c5c9129e2 *)

open Js_of_ocaml
open CommonUI
open Toppa

let relay_CK = "https://demo.httprelay.io/link/CK-c872be81-c6bc-5fa9-bb33-4e9c5c9129e2"
let relay_KC = "https://demo.httprelay.io/link/KC-c872be81-c6bc-5fa9-bb33-4e9c5c9129e2"

type 'a mesg = Move of 'a move | Game of 'a game

let send url m =
  let r = XmlHttpRequest.create () in
  r##_open (js "POST") (js url) Js._true;
  r##send (Js.Opt.return m)

let hash g = Printf.sprintf "[%d]" (Hashtbl.hash g)

let make incoming outgoing =
  let human =
    { visible = true;
      name = "Local Human";
      choose_move = fun x y ->
                    Drawer.message (hash x);
                    CommonUI.human.choose_move x y >>= fun m ->
                    send outgoing (Json.output (Move m));
                    Lwt.return m } in
  let rec ai =
    { visible = false;
      name = "Remote Human";
      choose_move = fun g _ -> (* Lwt.return (M.good_move g) *)
                    Drawer.message
                    @@ Printf.sprintf "%s is thinking... %s" ai.name (hash g);
                    let open Js_of_ocaml_lwt.XmlHttpRequest in
                    get incoming >>= fun f ->
                    match Json.unsafe_input (js f.content) with
                    | Game _ -> Lwt.fail_with "received a game, expecting a move"
                    | Move move ->
                       Drawer.message "";
                       Lwt.return move } in
  (human, ai)

(* let start () = *)
(*   let r = XmlHttpRequest.create () in *)
(*   r##_open (js "Post") (js "https://httprelay.io/link/gjtb8ehr") Js._true; *)
(*   r##send (Js.Opt.return ((Json.output (Toppa.init ())))) *)
(*   (\* r##.onload := Dom.handler (fun _ -> *\) *)
(*   (\*                   Js.Unsafe.eval_string (Printf.sprintf "window.alert('%s')" (Js.to_string r##.responseText)); *\) *)
(*   (\*                   Js._false) *\) *)

let start () =
  Random.self_init ();
  let body =
    Js.Opt.get (document##getElementById (js "yomi"))
               (fun () -> assert false) in
  body##.style##.cssText := js "background: #5F3E35; color: white";
  body##.innerHTML := js "Loading images...";
  let t = catch_raise @@ Drawer.wait_for_bank () in
  t >>= (fun () ->
    let human, ai = make relay_CK relay_KC in
    let rec loop b n acc =
      body##.innerHTML := js "";
      let s =
        Printf.sprintf "You have won %d point(s) after %d game(s).  \
                        You are the %s player in this round."
                       acc n (if b then "second" else "first") in
      Drawer.init s body;
      let g = init () in
      send relay_KC (Json.output (Game g));
      (* Drawer.make_place_human (if b then g.data.pii else g.data.pi).hand; *)
      Drawer.init_ba g.data.ba;
      Sound.init ();
      let module M = Make (struct let ai = ai let human = human end) in
      M.loop_play (if b then ai else human) g >>= fun (name, po) ->
      loop (not b) (n + 1) (acc + (if name = human.name then po else -po)) in
    loop false 0 0)

let _ =
  Html.window##.onload := Html.handler (fun _ -> ignore (start ()); Js._false)

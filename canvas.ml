open CommonUI
open Toppa

let start () =
  Random.self_init ();
  let body =
    Js.Opt.get (document##getElementById (js "yomi"))
               (fun () -> assert false) in
  body##.style##.cssText := js "background: #5F3E35; color: white";
  let t = catch_raise @@ Drawer.wait_for_bank () in
  let add_routine desc routine =
    let b0 = Html.createButton document in
    Dom.appendChild body b0;
    b0##.innerHTML := js desc;
    (catch_raise @@ Events.click b0) >>= fun _ ->
    Lwt.return routine in
  let t0 = add_routine "Easy AI" (`Worker "sigotoninEasy.js") in
  let t1 = add_routine "Standard AI" (`Worker "sigotoninStd.js") in
  let t2 =
    add_routine "Remote AI"
      (`Remote "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteToppa.cgi") in
  Lwt.pick [t0; t1; t2] >>= fun routine ->
  body##.innerHTML := js "Loading images...";
  t >>= (fun () ->
    let rec loop b n acc =
      body##.innerHTML := js "";
      let s =
        Printf.sprintf "You have won %d point(s) after %d game(s).  \
                        You are the %s player in this round."
                       acc n (if b then "second" else "first") in
      Drawer.init s body;
      let g = init () in
      (* Drawer.make_place_human (if b then g.data.pii else g.data.pi).hand; *)
      Drawer.init_ba g.data.ba;
      Sound.init ();
      let ai = make_ai "Computer" routine in
      let module M = Make (struct let ai = ai let human = human end) in
      M.loop_play (if b then ai else human) g >>= fun (name, po) ->
      loop (not b) (n + 1) (acc + (if name = human.name then po else -po)) in
    loop false 0 0)

(* let start () = *)
(*   let body = *)
(*     Js.Opt.get (document##getElementById (js "yomi")) *)
(*                (fun () -> assert false) in *)
(*   body##.style##.cssText := js "background: #008020"; *)
(*   Drawer.init body; *)
(*   Drawer.wait_for_bank () >>= (fun () -> *)
(*     Drawer.demo ()) *)

let _ =
  Html.window##.onload := Html.handler (fun _ -> ignore (start ()); Js._false)

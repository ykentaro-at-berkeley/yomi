open Js_of_ocaml
open CommonUI
open Toppa
let (relay_CK, relay_KC) = let open Ken in (relay_CK, relay_KC)

let start () =
  let body =
    Js.Opt.get (document##getElementById (js "yomi"))
               (fun () -> assert false) in
  body##.style##.cssText := js "background: #5F3E35; color: white";
  body##.innerHTML := js "Loading images...";
  let t = catch_raise @@ Drawer.wait_for_bank () in
  t >>= (fun () ->
    let human, ai = Ken.make relay_KC relay_CK in
    let rec loop b n acc =
      body##.innerHTML := js "";
      let s =
        Printf.sprintf "You have won %d point(s) after %d game(s).  \
                        You are the %s player in this round."
                       acc n (if b then "second" else "first") in
      Drawer.init s body;
      let open Js_of_ocaml_lwt.XmlHttpRequest in
      get relay_KC >>= fun { content } ->
      match Json.unsafe_input (js content) with
      | Ken.Move _ -> Lwt.fail_with "I received a move, expecting a game"
      | Ken.Game g ->
         Drawer.init_ba g.data.ba;
         Sound.init ();
         let module M = Make (struct let ai = ai let human = human end) in
         M.loop_play (if b then ai else human) g >>= fun (name, po) ->
         loop (not b) (n + 1) (acc + (if name = human.name then po else -po)) in
    loop true 0 0)

let _ =
  Html.window##.onload := Html.handler (fun _ -> ignore (start ()); Js._false)

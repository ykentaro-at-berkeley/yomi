module M = EdslCanvas.Make (RokutanGame)

let _ =
  let open Js_of_ocaml in
  Dom_html.window##.onload := Dom_html.handler (fun _ -> ignore (M.start ()); Js._false)

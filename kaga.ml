open EdslCanvas
open Js_of_ocaml

module M = EdslCanvas.Make (KagaGame)

let _ =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> ignore (M.start ()); Js._false)

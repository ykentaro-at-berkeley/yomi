open EdslNCanvas

module M = Make (ItinisanGame)

let _ =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> ignore (M.start ()); Js._false)
                                

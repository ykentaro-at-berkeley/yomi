open Mekuri

module M = MCUCB1(struct let limit = 1_0000 let param = 50. end)

let () =
  Worker.set_onmessage (fun s ->
      let game = Json.unsafe_input s in
      let m = M.good_move game in
      Worker.post_message @@ Json.output m)

open Toppa 

module Make (S : sig val limit : int end) = struct
  module M = MCUCB1(struct let limit = S.limit let param = 19. *. sqrt 2. end)

  let () =
    Worker.set_onmessage (fun s ->
        let game = Json.unsafe_input s in
        let m = M.good_move game in
        Worker.post_message @@ Json.output m)
end

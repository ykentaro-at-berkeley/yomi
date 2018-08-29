module M = struct
  type 'a game = 'a Toppa.game
  type 'a move = 'a Toppa.move
  module MCUCB1 = Toppa.MCUCB1 (struct let limit = 10000 let param = 19. *. sqrt 2. end)
  let packed_typerep_of_game = None
end
module R = Remote.Make (M)

open EdslN

let n_players = 3

let util_of_card = function
  | (Yanagi, Four) -> 10
  | (Yanagi, Three) -> 10
  | (Yanagi, Two) -> 5
  | (Kiri, Three) | (_, Two) | (_, One) -> 30
  | (Susuki, Three) -> 10
  | (_, Three) -> 5
  | _ -> 10

let yaku = []

let yaku_join_type = Before_megati
let rule = Abort_when_yaku
let oni_type = Leave_rest
let is_oni _ _ = false
let n_te = 7
let n_ba = 6
let handle_sarasi _ _ = (0, [])
let deal_type = No_basi
let megati = Megati_without_origin 1
let remote_url = ""
module UCB1 = struct
  let limit = 3000
  let param = sqrt 2.
end

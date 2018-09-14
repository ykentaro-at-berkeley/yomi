open Edsl
let is_oni _ _ = false

let util_of_card = function
  | (Yanagi, Four) -> 5
  | (Yanagi, Three) -> 0
  | (Yanagi, Two) -> 1
  | (Yanagi, One) -> 0
  | (Kiku, Four) -> 9
  | (Susuki, Four) -> 8
  | (Hagi, Four) -> 7
  | (Botan, Four) -> 6
  | (_, Four) -> 5
  | (_, Three) -> 1
  | _ -> 0

let yaku = []

let yaku_type = Simple
let yaku_join_type = Conditional
let rule = Abort_when_yaku
let payoff_type = Difference
let oni_type = Leave_rest
let n_ba = 8
let n_te = 8

let handle_sarasi _ _ =  0, []

let deal_type = No_basi
let megati = Megati_none
let bound = None

let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteSanuki.cgi"

module UCB1 = struct
  let limit = 10000
  let param = 40. *. sqrt 2.
end

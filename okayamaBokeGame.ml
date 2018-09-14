open Edsl
let is_oni _ _ = false

let util_of_card = function
  | (Yanagi, Four) -> 20
  | (Yanagi, Three) -> 10
  | (Yanagi, Two) -> 5
  | (Yanagi, One) -> 0
  | (Kiri, Four) -> 20
  | (Kiri, Three) -> 10
  | (Susuki, Four) -> 20
  | (Susuki, Three) -> 10
  | (Matu, Four) | (Ume, Four) | (Sakura, Four)  | (Momizi, Four) -> 20
  | (_, Four) -> 10
  | (_, Three) -> 5
  | _ -> 0

let yaku = []

let yaku_type = Simple
let yaku_join_type = Conditional
let rule = Abort_when_yaku
let payoff_type = Difference
let oni_type = Leave_rest
let n_ba = 6
let n_te = 9

let handle_sarasi _ _ =  0, []

let deal_type = No_basi
let megati =
  let f (u, u') = (max 0 (u - 135), max 0 (u' - 135)) in
  Megati_fun f
let bound = None

let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteOkayamaBoke.cgi"

module UCB1 = struct
  let limit = 10000
  let param = 100. *. sqrt 2.
end

(* 3-5 no baka-rokutan, sudaosi no huda-ten *)
open Edsl
let is_oni _ _ = false

let util_of_card = function (* Sudaosi apud Simizu *)
  | (Yanagi, Four) -> 5
  | (Yanagi, Three) -> 5
  | (Yanagi, Two) -> 1
  | (Kiri, Three) | (_, Two) | (_, One) -> 10
  | (Susuki, Three) -> 5
  | (_, Three) -> 1
  | _ -> 5

let is_tan'  = function
  | (Yanagi, _) -> true
  | c -> util_of_card c = 1
let ntan = "Rokutan", Count (5, 6, is_tan')

let yaku = [ntan]

let yaku_type = Simple
let yaku_join_type = Conditional
let rule = Abort_when_yaku
let payoff_type = Difference
let oni_type = Leave_rest
let n_ba = 8
let n_te = 8

let handle_sarasi _ _ =  0, []

let deal_type = No_basi
let megati = Megati_thru 3
let bound = None

let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteRokutan.cgi"

module UCB1 = struct
  let limit = 10000
  let param = 5. *. sqrt 2.
end

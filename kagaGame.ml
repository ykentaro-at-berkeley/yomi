open Edsl
let is_oni _ _ = false

let util_of_card = function
  | (Yanagi, Four) | (Kiri, Four) -> 20
  | (Kiri, _) -> 5
  | (Yanagi, Three) -> 10
  | (Yanagi, Two) -> 5
  | _ -> 0

let f_sikoo = function
  | (Matu, Four) | (Sakura, Four) | (Susuki, Four) | (Kiri, Four) -> true
  | _ -> false
let sikoo = "Sikô", Count (100, 4, f_sikoo)

let f_aotan = function
  | (Botan, Three) | (Kiku, Three) | (Momizi, Three) -> true
  | _ -> false
let aotan = "Aotan", Count (100, 3, f_aotan)

let f_akatan = function
  | (Matu, Three) | (Ume, Three) | (Sakura, Three) -> true
  | _ -> false
let akatan = "Akatan", Count (100, 3, f_akatan)

let make_sima u m =
  let f (m', _) = m' == m in
  "Sima", Count (u, 4, f)

let yaku =
  [sikoo; aotan; akatan]
  @ (List.map (fun m -> make_sima 20 m)
       [Matu; Ume; Sakura; Huzi; Ayame; Botan; Hagi; Susuki; Kiku; Momizi;
        Yanagi; Kiri])

let yaku_type = Simple
let yaku_join_type = Sum
let rule = Till_end
let payoff_type = Difference
let oni_type = Leave_rest
let n_ba = 8
let n_te = 8

let handle_sarasi _ _ =  0, []

let deal_type = No_basi
let bound = None
let megati = Megati_none

module UCB1 = struct
  let limit = 3000
  let param = 610.0
end
    

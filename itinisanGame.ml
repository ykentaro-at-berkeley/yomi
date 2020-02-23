open EdslN

let n_players = 3

let util_of_card = function
  | Matu, Four | Sakura, Four | Susuki,Four | Yanagi, Four | Kiri, Four -> 20
  | _, Four | Susuki, Three | Yanagi, Three -> 10
  | Yanagi, Two -> 5
  | m, Three when m != Kiri -> 5
  | _ -> 1

let f_sikoo = function
  | (Matu, Four) | (Sakura, Four) | (Susuki, Four) | (Kiri, Four) -> true
  | _ -> false
let sikoo = "Sikô", Count (3, 4, f_sikoo)

let f_aotan = function
  | (Botan, Three) | (Kiku, Three) | (Momizi, Three) -> true
  | _ -> false
let aotan = "Aotan", Count (2, 3, f_aotan)

let f_akatan = function
  | (Matu, Three) | (Ume, Three) | (Sakura, Three) -> true
  | _ -> false
let akatan = "Akatan", Count (2, 3, f_akatan)

let yaku = [sikoo; aotan; akatan]

let yaku_join_type = After_megati
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
  let limit = 1000
  let param = 6. *. sqrt 2.
end
                

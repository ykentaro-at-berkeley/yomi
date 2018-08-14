open Edsl
let is_oni _ = function
  | Matu, Four | Kiri, Four | Susuki, Four -> true
  | _ -> false

let util_of_card = function
  | c when is_oni `Te c -> 100
  | Sakura, Four | Yanagi, Four -> 20
  | _, Four | Susuki, Three | Yanagi, Three -> 10
  | Yanagi, Two -> 5
  | m, Three when m != Kiri -> 5
  | _ -> 0

let f_sikoo = function
  | (Matu, Four) | (Sakura, Four) | (Susuki, Four) | (Kiri, Four) -> true
  | _ -> false
let sikoo = "Sikô", Count (100, 4, f_sikoo)

let f_itinisan = function
  | (Matu, Four) | (Ume, Four) | (Sakura, Four) -> true
  | _ -> false
let itinisan = "Itinisan", Count (50, 3, f_itinisan)

let f_aotan = function
  | (Botan, Three) | (Kiku, Three) | (Momizi, Three) -> true
  | _ -> false
let aotan = "Aotan", Count (50, 3, f_aotan)

let f_akatan = function
  | (Matu, Three) | (Ume, Three) | (Sakura, Three) -> true
  | _ -> false
let akatan = "Akatan", Count (40, 3, f_akatan)

let f_botankiri = function
  | (Botan, Four) | (Huzi, Four) | (Ayame, Four) -> true
  | _ -> false
let botankiri = "Botankiri", Count (30, 3, f_botankiri)

let f_ganbo = function
  | (Hagi, Four) | (Momizi, Four) | (Susuki, Three) -> true
  | _ -> false
let ganbo = "Ganbo", Count (30, 3, f_ganbo)

let yaku = [sikoo; itinisan; aotan; akatan; botankiri; ganbo]

let yaku_type = Simple
let yaku_join_type = Conditional
let rule = Abort_when_yaku
let payoff_type = Difference
let megati = Megati_none
let oni_type = Leave_rest
let n_ba = 8
let n_te = 8

let handle_sarasi _ cs =
  0, List.filter (is_oni `Te) cs

let deal_type = No_basanbon
let bound = Some 200                  

module UCB1 = struct
  let limit = 2000
  let param = 200.
end

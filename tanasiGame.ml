open Edsl
let is_oni _ _ = false

let util_of_card = function
  | Matu, Four | Kiri, Four | Susuki, Four | Sakura, Four | Yanagi, Four -> 20
  | _, Four | Susuki, Three | Yanagi, Three -> 10
  | Yanagi, Two -> 5
  | m, Three when m != Kiri -> 5
  | _ -> 0

let f_gokoo c = 20 == util_of_card c
let gokoo = "Ômonozoroi", Count (1, 5, f_gokoo)

let f_aotan = function
  | (Botan, Three) | (Kiku, Three) | (Momizi, Three) -> true
  | _ -> false
let aotan = "Ao Ura", Count (1, 3, f_aotan)

let f_ao_omote = function
  | (Botan, Four) | (Kiku, Four) | (Momizi, Four) -> true
  | _ -> false
let ao_omote = "Ao Omote", Count (1, 3, f_ao_omote)

let f_akatan = function
  | (Matu, Three) | (Ume, Three) | (Sakura, Three) -> true
  | _ -> false
let akatan = "Aka Ura", Count (1, 3, f_akatan)

let f_aka_omote = function
  | (Matu, Four) | (Ume, Four) | (Sakura, Four) -> true
  | _ -> false
let aka_omote = "Aka Omote", Count (1, 3, f_aka_omote)

let make_sima s m =
  let f (m', _) = m' == m in
  (s ^ " Zoro"), Count (1, 4, f)

let yaku =
  [gokoo; aotan; ao_omote; akatan; aka_omote]
  @ (List.map (fun (s, m) -> make_sima s m)
       [("Matu", Matu); ("Sakura", Sakura); ("Bôzu", Susuki); ("Kiri", Kiri)])

let yaku_type = Simple
let yaku_join_type = Conditional
let rule = Abort_when_yaku
let payoff_type = Difference
let oni_type = Leave_rest
let n_ba = 8
let n_te = 8

let handle_sarasi _ _ =  0, []

let deal_type = No_basi
let bound = Some 1 (* To cope with the awase2 problem *)
let megati = Megati_thru 1

let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteTanasi.cgi"

module UCB1 = struct
  let limit = 10000
  let param = sqrt 2.
end
    

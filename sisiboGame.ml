open EdslN

let n_players = 3

let util_of_card = function
  | Matu, Four | Sakura, Four | Susuki,Four (* | Yanagi, Four *) | Kiri, Four -> 20
  | _, Four | Susuki, Three | Yanagi, Three -> 10
  | Yanagi, Two -> 5
  | m, Three when m != Kiri -> 5
  | _ -> 0

let sikoo = ItinisanGame.sikoo
let akatan = ItinisanGame.akatan
let aotan = ItinisanGame.aotan

let pred fl =  List.mem `Thru fl
let f_tukihoto = function
  | (Susuki, Four) | (Huzi, Four) -> true
  | _ -> false
let tukihoto = "Tukihoto", With_pred (pred, Count (50, 2, f_tukihoto))
let f_sisibo = function
  | (Hagi, Four) | (Botan, Four) -> true
  | _ -> false
let sisibo = "Sisibo", With_pred (pred, Count (50, 2, f_sisibo))
let f_sisityuu = function
  | (Hagi, Four) | (Yanagi, Four) -> true
  | _ -> false
let sisityuu = "Sisityuu", With_pred (pred, Count (50, 2, f_sisityuu))

let make_sima s m =
  let f (m', _) = m' == m in
  (s ^ "-Sima"), With_pred (pred, Count (50, 4, f))
let negi_sima = make_sima "Negi" Ayame
let kiku_sima = make_sima "Kiku" Kiku
let kiri_sima = make_sima "Kiri" Kiri
let yaku = [sikoo; akatan; aotan; tukihoto; sisibo; sisityuu;
            negi_sima; kiku_sima; kiri_sima ]

let rule = Abort_when_yaku
let yaku_join_type = Before_megati
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
             


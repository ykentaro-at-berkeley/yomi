open Edsl
let is_oni use card =
  match use, card with
  | `Te, (Yanagi, One) | `Mekuri, (Yanagi, One) -> true (* How about ba? *)
  | _ -> false

let util_of_card = function
  | (Yanagi, Four) -> 5
  | (Yanagi, Three) -> 5
  | (Yanagi, Two) -> 10
  | (Yanagi, One) -> 0
  | (Kiri, Four) -> 20
  | (Kiri, Three) -> 10
  | (Susuki, Four) -> 20
  | (Susuki, Three) -> 5
  | (Matu, Four) | (Sakura, Four) -> 20
  | (_, Four) -> 5
  | (_, Three) -> 10
  | _ -> 0

let f_nomi = function
  | (Sakura, Four) | (Kiku, Four) | (Susuki, Four) -> true
  | _ -> false
let nomi = ("Nomi", Count (100, 3, f_nomi))

let f_ino_sika_gan = function
  | (Hagi, Four) | (Momizi, Four) | (Susuki, Three) -> true
  | _ -> false
let ino_sika_gan = ("Ino-Sika-Gan", Count (100, 3, f_ino_sika_gan))

let make_omote_ura s x y z =
  let f m = (m = x || m = y || m = z) in
  let f_omote (m, r) = r = Four && f m in
  let f_ura (m, r) = r = Three && f m in
  ((s ^ " Omote", Count (100, 3, f_omote)), (s ^ " Ura", Count (100, 3, f_ura)))

let aka_omote, aka_ura = make_omote_ura "Aka" Sakura Ume Matu
let ao_omote, ao_ura = make_omote_ura "Ao" Botan Kiku Momizi
let kusa_omote, kusa_ura = make_omote_ura "Kusa" Huzi Ayame Hagi

let yaku = [nomi; ino_sika_gan;
            aka_omote; aka_ura;
            ao_omote; ao_ura;
            kusa_omote; kusa_ura]

let yaku_type = Simple (* Deiri is unimplemented! *)
let yaku_join_type = Sum 
let rule = Till_end
let payoff_type = Absolute
let oni_type = Leave_rest (* The correct rule is unclear *)
let n_ba = 8 (* ditto *)
let n_te = 8

let handle_sarasi _ _ =  0, []

let deal_type = No_basanbon
let megati =
  let rec f (u, u') =
    if u = u' then (0, 0)
    else if u < u' then
      let x, y = f (u', u) in (y, x)
    else
      ((if u - u' >= 100 then 2 else 1), 0) in
  Megati_fun f
let bound = None

let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteHigo.cgi"

module UCB1 = struct
  let limit = 10000
  let param = 100. *. sqrt 2.
end

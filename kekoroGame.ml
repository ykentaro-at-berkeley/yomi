open Edsl

let (@) = List.rev_append

let util_of_card = RokutanGame.util_of_card (* sudaosi *)

let make_triple ?(pv = `Ikimono) s x y z : yaku=
  let f' mon = mon == x || mon == y || mon == z in
  let f = 
    match pv with
    | `Ikimono ->
       (function
        | (mon, Four) -> f' mon
        | _ -> false)
    | `Tan ->
       (function
        | (Yanagi, Two) -> f' Yanagi
        | (Yanagi, _) -> false
        | (mon, Three) -> f' mon
        | _ -> false) in
  (s, Count (30, 3, f))
let make_both s x y z =
  [make_triple s x y z; make_triple ~pv:`Tan ("Tan no " ^ s) x y z]

let yaku =
  make_both "Kirisima" Ume Huzi Kiri
  @ make_both "Gundari" Ume Kiri Momizi
  @ make_both "5 6 10" Momizi Ayame Botan
  @ make_both "3 5 6" Ayame Botan Sakura
  @ make_both "5 4 6" Ayame Huzi Botan
  @ make_both "7 5 3" Hagi Ayame Sakura
  @ make_both "Kumanosan" Yanagi Sakura Kiku (* Why yanagi? *)
  @ make_both "Yasima" Susuki Yanagi Huzi
  @ make_both "Nakazô" Susuki Kiku Hagi
  @ [make_triple ~pv:`Tan "4 7 8" Hagi Susuki Huzi;
     make_triple ~pv:`Tan "10 7 8" Hagi Susuki Momizi]
  @ make_both "Ebi" Momizi Yanagi Matu
  @ make_both "Simozan" Sakura Yanagi Matu
  @ make_both "Deame" Yanagi Hagi Susuki

let yaku_type = Simple
let yaku_join_type = Sum
let rule = Till_end
let payoff_type = Difference
let oni_type = Leave_rest
let is_oni _ _ = false
let n_te = 8
let n_ba = 8
let handle_sarasi _ _ = (0, [])
let deal_type = No_basi
let bound = None
let megati = Megati_none
let remote_url = "https://www.ocf.berkeley.edu/~ykentaro/yomi/remoteKekoro.cgi"

module UCB1 = struct
  let limit = 10000
  let param = 90. *. sqrt 2.
end
                

open Mekuri

let string_of_month = function
  | 1 -> "matu"
  | 2 -> "ume"
  | 3 -> "sakura"
  | 4 -> "huzi"
  | 5 -> "ayame"
  | 6 -> "botan"
  | 7 -> "hagi"
  | 8 -> "susuki"
  | 9 -> "kiku"
  | 10 -> "momizi"
  | 11 -> "yanagi"
  | 12 -> "kiri"
  | _ -> invalid_arg "string_of_month month out of bounds"

let string_of_card c =
  let cat x m = x ^ " of " ^ (string_of_month m) in
  match classify_card c with
  | CExist (Hikari_card (m, _)) -> cat "hikari" m
  | CExist (Tane_card (m, _)) -> cat "tane" m
  | CExist (Tanzaku_card (m, _)) -> cat "tanzaku" m
  | CExist (Kasu_card (m, _)) -> cat "kasu" m

let string_of_yaku y =
  let f = function
    | Gokoo -> "Gokô"
    | Sikoo -> "Sikô"
    | Sankoo -> "Sankô"
    | Amesikoo -> "Amesikô"
    | Akatan -> "Akatan"
    | Aotan -> "Aotan"
    | Tanzaku _ -> "Tanzaku"
    | Inosikatyoo -> "Ino-sika-tyô"
    | Tane _ -> "Tane"
    | Kasu _ -> "Kasu" in
  Printf.sprintf "%s (%d)" (f y) (util_of_yaku y)

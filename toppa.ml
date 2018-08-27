(* 9 point(s) after 120 game(s). *)

(* http://www.geocities.jp/xmbwq497/gihou/hanafuda-gihou1.html *)
open MyStd

let (@) = List.rev_append

type month = int
type rank = One | Two | Three | Four
type card' = month * rank
type hikari_t = XHikari
type tanzaku_t = XTanzaku
type tane_t = XTane
type kara_t = XKara
type _ card =
  | Hikari_card : card' -> hikari_t card (* including the aristo *)
  | Tanzaku_card : card' -> tanzaku_t card (* including the three willows *)
  | Tane_card : card' -> tane_t card
  | Kara_card : card' -> kara_t card
type ecard = CExist : 'a card -> ecard

type tori =
  { hikari : hikari_t card list;
    tanzaku : tanzaku_t card list;
    tane :  tane_t card list;
    kara :  kara_t card list;
    has_aristo : bool;
    has_oni : bool }

type yaku =
  | Gokoo | Sikoo | Amesikoo 
  | Akatan | Aotan | Nanatan of int (* excess # of cards *)
  | Kara of int (* ditto *)

(* Do not want to use polymorphic eq function *)
(* Idk why, but = emits that for rank *)
let eq_card' ((m, r) : card') ((m', r') : card') = m == m' && r == r'
let eq_card : type a. a card -> a card -> bool =
  fun x y ->
  match x, y with
  | Hikari_card c, Hikari_card c' -> eq_card' c c'
  | Tanzaku_card c, Tanzaku_card c' -> eq_card' c c'
  | Tane_card c, Tane_card c' -> eq_card' c c'
  | Kara_card c, Kara_card c' -> eq_card' c c'

let aristocrat = Hikari_card (11, Four)

let yaku_of_hikari has_aristo (cs : hikari_t card list) =
  match List.length cs with
  | 5 -> [Gokoo]
  | 4 when has_aristo -> [Amesikoo]
  | 4 -> [Sikoo]
  | _ -> []

let is_ao (Tanzaku_card (m, _)) =
  match m with
  | 6 | 9 | 10 -> true
  | _ -> false

let is_aka (Tanzaku_card (m, _)) = 1 <= m  && m <= 3

let yaku_of_tanzaku has_aristo (cs : tanzaku_t card list) =
  let l, n_ao, n_aka =
    let f (l, n_ao, n_aka) c =
      (l + 1,
       n_ao + (if is_ao c then 1 else 0),
       n_aka + (if is_aka c then 1 else 0)) in
    List.fold_left f (0, 0, 0) cs in
  let l = if has_aristo then l + 1 else l in
  (if n_ao >= 3 then [Aotan] else [])
  @ (if n_aka >= 3 then [Akatan] else [])
  @ (if l >= 7 then [Nanatan (l - 7)] else [])

(* One could add innosikatyoo ... *)

(* let ino = Tane_card (7, Four) *)
(* let sika = Tane_card (10, Four) *)
(* let tyoo = Tane_card (6, Four) *)
(* let yaku_of_tane (cs : tane_t card list) = *)
(*   let (l, has_ino, has_sika, has_tyoo) =  *)
(*     let f (l, has_ino, has_sika, has_tyoo) c = *)
(*       (l + 1, *)
(*        has_ino || eq_card c ino, *)
(*        has_sika || eq_card c sika, *)
(*        has_tyoo || eq_card c tyoo) in *)
(*     List.fold_left f (0, false, false, false) cs in *)
(*   (if has_ino && has_sika && has_tyoo then [Inosikatyoo] else []) *)
(*   @ (if l >= 5 then [Tane (l - 5)] else []) *)

let yaku_of_kara has_oni (cs : kara_t card list)  =
  let l = List.length cs + (if has_oni then 1 else 0) in
  if l >= 12 then  [Kara (l - 12)] else []

let yaku_of_tori t =
  yaku_of_hikari t.has_aristo t.hikari
  @ yaku_of_tanzaku t.has_aristo t.tanzaku
  (* @ yaku_of_tane t.tane *)
  @ yaku_of_kara t.has_oni t.kara 

type util = int

let util_of_yaku = function
  | Gokoo -> 3
  | Sikoo -> 2
  | Amesikoo -> 1
  | Akatan -> 1
  | Aotan -> 1
  | Nanatan n -> 2 + n
  | Kara n -> 1 + n

let util_of_tori t =
  let f acc c = acc + util_of_yaku c in
  List.fold_left f 0 @@ yaku_of_tori t

let is_month_with_hikari = function
  | 1 | 3 | 8 | 11 | 12 -> true
  | _ -> false

let classify_card c =
  match c with 
  | (11, Four) -> CExist (Hikari_card c)
  | (11, _) -> CExist (Tanzaku_card c)
  | (m, Four) ->
     if is_month_with_hikari m then CExist (Hikari_card c)
     else CExist (Tane_card c)
  | (m, Three) ->
     begin
       match m with
       | 8 | 11 -> CExist (Tane_card c)
       | 12 -> CExist (Kara_card c)
       | _ -> CExist (Tanzaku_card c)
     end
  | (m, Two) ->
     if m = 11 then CExist (Tanzaku_card c)
     else CExist (Kara_card c)
  | (_, One) -> CExist (Kara_card c)

let is_aristo' = function
  | (11, Four) -> true
  | _ -> false

let is_oni' = function
  | (11, One) -> true
  | _ -> false

let cons (c' : card') (t : tori) =
  let CExist c = classify_card c' in
  match c with
  | Hikari_card _ -> { t with hikari = c::t.hikari; 
                              has_aristo = t.has_aristo || is_aristo' c' }
  | Tane_card _ -> { t with tane = c::t.tane }
  | Tanzaku_card _ -> { t with tanzaku = c::t.tanzaku;
                               has_oni = t.has_oni || is_oni' c' }
  | Kara_card _ -> { t with kara = c::t.kara }
  

type player = { hand : card' list; tori : tori; koi : tori option }

type player_id = PI | PII

let other = function PI -> PII | PII -> PI

type game' =
  { current : player_id;
    ba : card' list;
    yama : card' list;
    (* sum_hands : int; *)
    pi : player;
    pii : player }
    
type play_t
type koi_t
type awase1_t
type awase2_t
type winning_t
type draw_t
type _ game_phase =
  | Play_phase : play_t game_phase
  | Koi_phase : koi_t game_phase
  | Awase1_phase : card' -> awase1_t game_phase (* lastly played card *)  
  | Awase2_phase : card' -> awase2_t game_phase (* lastly drawn card *)
  | Winning_phase : winning_t game_phase
  | Draw_phase : draw_t game_phase

type 'a game =
  { phase : 'a game_phase;
    data : game' }

let player_of_game' { current = c; pi = pi; pii = pii } =
  match c with
  | PI -> pi
  | PII -> pii
let player_of_game g = player_of_game' g.data

let update_current_player' ({ current = c } as g') p =
  match c with
  | PI -> { g' with pi = p }
  | PII -> { g' with pii = p }
let update_current_player g p =
  { g with data = update_current_player' g.data p }
      
let swap' data =
  { data with current = other data.current }
let swap g = { g with data = swap' g.data }

type _ move =
  | Play : card' -> play_t move
  | Koi : koi_t move
  | No_koi : koi_t move
  | Awase1 : card' -> awase1_t move
  | Awase1_nop : awase1_t move
  | Awase1_basanbon : card' * card' * card' -> awase1_t move
  | Awase2 : card' * card' -> awase2_t move (* The card drawn comes first *)
  | Awase2_basanbon : card' * card' * card' * card' -> awase2_t move
  | Awase2_nop : card' -> awase2_t move

let awase ((m, r) : card') ba =
  let f acc ((m', r') as c') = if m' = m then c' :: acc else acc in 
  List.fold_left f [] ba

let moves : type a. a game -> a move list =
  function
  | { phase = Awase1_phase c; data = data } ->
     begin
       match awase c data.ba with
       | [] -> [Awase1_nop]
       | x::y::z::w::_ -> failwith "moves: too many matching cards"
       | x::y::z::[] -> [Awase1_basanbon (x, y, z)]
       | x::y::[] -> [Awase1 x; Awase1 y]
       | x::[] -> [Awase1 x]
       (* | cs -> List.map (fun c' -> Awase1 c') cs *)
     end
  | { phase = Awase2_phase c; data = data } -> 
     begin
       match awase c data.ba with
       | [] -> [Awase2_nop c]
       | x::y::z::w::_ -> failwith "moves: too many matching cards"
       | x::y::z::[] -> [Awase2_basanbon (c, x, y, z)]
       | cs -> List.map (fun c' -> Awase2 (c, c')) cs
     end
  | { phase = Koi_phase; data = data } ->
     begin
       match (player_of_game' data).hand with
       | [] -> [No_koi]
       | _ -> [Koi; No_koi]
     end
  | { phase = Play_phase } as g ->
     let p = player_of_game g in
     List.map (fun c -> Play c) p.hand
  | { phase = Winning_phase } -> failwith "moves: applied to a winning game"
  | { phase = Draw_phase } -> failwith "moves: applied to a draw game"

(* internal definitions should use util_of_tori instead *)
let payoff' g : float option =
  let payoff' { tori = tori } =
    let u = util_of_tori tori in
    if u > 0 then Some (float u) else None in
    (* let y = yaku_of_tori tori in *)
    (* match y with *)
    (* | [] -> None *)
    (* | y ->  *)
    (*    let f acc c = acc + util_of_yaku c in *)
    (*    Some (float (List.fold_left f 0 y)) in *)
  let help cur opp =
    match (payoff' cur) with
    | Some po -> Some po
    | None ->
       match payoff' opp with
       | Some po -> Some (~-. po)
       | None -> None in
  (* match g.pi.hand, g.pii.hand with *)
  (* | [], [] -> Some 0.0 (\* implies worthless koi-nagare *\) *)
  (* | _ -> *)
     match g.current with
     | PI -> help g.pi g.pii
     | PII -> help g.pii g.pi

(* TODO: payoff is None after Koi (but Some _ after No_koi)  *)
(* Use GADT *)
(* evaluates to None at Koi_phase *)
let payoff : type a. a game -> float option
  = function
  | { phase = Winning_phase; data = data } -> payoff' data
  | { phase = Draw_phase } ->  Some 0.0 (* worthless koi-nagare *)
  | _ -> None

type egame = GExist : 'a game -> egame

(* From containers *)
(* monomorphic list operations *) 
let remove_card' x l =
  let rec remove' x acc l = match l with
    | [] -> List.rev acc
    | y :: tail when eq_card' x y -> remove'  x acc tail
    | y :: tail -> remove'  x (y::acc) tail
  in
  remove'  x [] l
let diff_card' (xs : card' list) (ys : card' list) =
  let f xs y = remove_card' y xs in
  List.fold_left f xs ys
  
let apply_awase1_ { phase = Awase1_phase c; data = data } m =
  match data.yama with
  | [] -> failwith "apply : we should never run out of cards"
  | top::yama ->  
     let data = { data with yama = yama } in
     let p = player_of_game' data in
     let help tori cs =
       let p = { p with tori = tori } in
       let data = update_current_player' data p in
       { data with ba = diff_card' data.ba cs } in
     begin
       match m with
       | Awase1_nop ->
          { phase = Awase2_phase top;
            data = { data with ba = c::data.ba }}
       | Awase1 c' ->
          let tori = cons c (cons c' p.tori) in
           { phase = Awase2_phase top; data = help tori [c'] }
       | Awase1_basanbon (c', c'', c''') ->
          let tori = cons c (cons c' (cons c'' (cons c''' p.tori))) in
           { phase = Awase2_phase top; data = help tori [c'; c''; c''']}
     end

let check_draw ({ phase = Play_phase; data = data } as g) =
  match data.pi.hand, data.pii.hand with
  | [], [] -> GExist { phase = Draw_phase; data = data}
  | _ -> GExist g

let apply_awase2 { phase = Awase2_phase c; data = data } m =
  let p = player_of_game' data in
  let help tori cs =
    let p = { p with tori = tori } in
    let data = update_current_player' data p in
    { data with ba = diff_card' data.ba cs } in
  let k data =
    let flag =
      let p = (player_of_game' data) in
      let u = util_of_tori p.tori in
      if u > 0 then
        match p.koi with
        | None -> true
        | Some t when u > util_of_tori t -> true (* Is this OK? *)
        | _ -> false
      else false in
    (* Printf.printf "The flag is %b\n" flag; *)
    if flag then GExist { phase = Koi_phase; data = data }
    else (check_draw { phase = Play_phase; data = swap' data }) in
  match m with
  | Awase2_nop _ -> (* need to add it *)
     k { data with ba = c::data.ba }
  | Awase2 (_, c') ->
     let tori = cons c (cons c' p.tori) in
     k (help tori [c'])
  | Awase2_basanbon (_, c', c'', c''') ->
     let tori = cons c (cons c' (cons c'' (cons c''' p.tori))) in
     k (help tori [c'; c''; c'''])

let apply_play_ { data = data } (Play c) =
  let p = player_of_game' data in
  (* let l = List.length p.hand in *)
  let p = { p with hand = remove_card' c p.hand } in
  (* assert (List.length p.hand = l - 1); *)
  { phase = Awase1_phase c; data = update_current_player' data p }

let apply_koi { data = data } = function
  | Koi ->
     let p = player_of_game' data in
     let data = update_current_player' data { p with koi = Some (p.tori) } in
     (check_draw { phase = Play_phase; data = swap' data })
  | No_koi -> GExist { phase = Winning_phase; data = data } (* Do not swap *)

let apply : type a. a game -> a move -> egame
  = fun g m ->
  match g with
  | { phase = Awase1_phase _ } as g -> GExist (apply_awase1_ g m)
  | { phase = Awase2_phase _} as g -> apply_awase2 g m
  | { phase = Play_phase } as g -> GExist (apply_play_ g m)
  | { phase = Koi_phase } as g -> (apply_koi g m)
  | { phase = Winning_phase } ->
     failwith "apply: applied to a winning state"
  | { phase = Draw_phase } -> failwith "apply: applied to a draw game"

let cards_of_tori t =
  let a = List.map (fun (Hikari_card c) -> c) t.hikari in
  let b = List.map (fun (Tane_card c) -> c) t.tane in
  let c = List.map (fun (Tanzaku_card c) -> c) t.tanzaku in
  let d = List.map (fun (Kara_card c) -> c) t.kara in
  a @ b @ c @ d 

let hana_karuta : card' list =
  List.flatten
  @@ List.map (fun r ->
         List.map (fun m -> (m, r)) [1;2;3;4;5;6;7;8;9;10;11;12])
              [One;Two;Three;Four]

let has_si hand =
  let rec loop m =
    if m > 12 then false
    else
      if
        let f acc (m', _) = if m = m' then 1 + acc else acc in
        4 <= List.fold_left f 0 hand
      then true
      else loop (1 + m) in
  loop 1

let rec take_drop_wo_si n xs0 =
  let xs, xs' = List.take_drop n xs0 in
  if has_si xs then take_drop_wo_si n (Random.shuffle_list xs0)
  else xs, xs'

(* create a random game based on the current player's perspective *)
let random : type a. a game -> a game =
  fun g ->
  let p = player_of_game g in
  let cs_tori = cards_of_tori p.tori in
  let p' = (player_of_game (swap g)) in
  let cs_tori' = cards_of_tori p'.tori in
  let additional =
    match g with
    | { phase = Awase1_phase c } -> [c]
    | { phase = Awase2_phase c } -> [c]
    | _ -> [] in
  let visible = additional @ p.hand @ g.data.ba @ cs_tori @ cs_tori' in
  let cs = diff_card' hana_karuta visible in
  let cs = Random.shuffle_list cs in
  let hand, cs = take_drop_wo_si (List.length p'.hand) cs in (* I'm not cheating! *)
  assert (List.length cs = List.length g.data.yama);
  let p' = { p' with hand = hand } in
  let g = { g with data = { g.data with yama = cs } } in
  swap (update_current_player (swap g) p')

let deal n =
  let cs = Random.shuffle_list hana_karuta in
  let ba, cs = take_drop_wo_si 8 cs in
  let rec loop a i cs =
    if i >= n then a
    else
      let hand, cs = take_drop_wo_si 10 cs in
      loop ((ba, hand) :: a) (i + 1) cs in
  loop [] 0 cs

let join (ba, hand) (ba', hand') =
  assert (ba = ba');
  let empty =
    { hikari = []; tane = []; tanzaku = []; kara = [];
      has_aristo = false; has_oni = false } in
  let pi = { hand = hand; tori = empty; koi = None } in
  let pii = { hand = hand'; tori = empty; koi = None } in
  let yama = Random.shuffle_list (diff_card' hana_karuta (ba @ hand @ hand')) in
  let data = { pi = pi; pii = pii; ba = ba; yama = yama; current = PI } in
  { phase = Play_phase; data = data }
  
let init_from (ba, hand) =
  let cs = diff_card' hana_karuta (ba @ hand) in
  let cs = Random.shuffle_list cs in
  let hand', _ = take_drop_wo_si 10 cs in (* Toppa!!!! *)
  join (ba, hand) (ba, hand')

let init () =
  let cs = Random.shuffle_list hana_karuta in
  let ba, cs = take_drop_wo_si 8 cs in
  let hand, _ = take_drop_wo_si 10 cs in
  init_from (ba, hand)

(* let init () = *)
(*   let empty = *)
(*     { hikari = []; tane = []; tanzaku = []; kara = []; has_aristo = false  } in *)
(*   let cs = Random.shuffle_list hana_karuta in *)
(*   let hand, cs = take_drop_wo_si 10 cs in (\* Toppa!!!! *\) *)
(*   let pi = { hand = hand; tori = empty; koi = None } in *)
(*   let hand, cs = take_drop_wo_si 10 cs in (\* Toppa!!!! *\) *)
(*   let pii = { hand = hand; tori = empty; koi = None } in *)
(*   let ba, yama = take_drop_wo_si 8 cs in *)
(*   let data = { pi = pi; pii = pii; ba = ba; yama = yama; current = PI } in *)
(*   { phase = Play_phase; data = data } *)

    

module MCUCB1 (P : sig val param : float val limit : int end) = struct
  let param = P.param
  let limit = P.limit
    
  let rec simple_playout : type a. player_id -> a game -> a move -> float =
    fun p g m ->
    (* begin *)
    (*   match m with *)
    (*   | Play (m, r) -> *)
    (*      Printf.printf "(%d, %d)\n" m (Obj.magic r) *)
    (*   | _ -> () *)
    (* end; *)
    let GExist g = apply g m in
    match payoff g with
    | Some po ->
         if g.data.current == p then po else ~-.po
    | None ->
       let ms = moves g in
       simple_playout p g (Random.randomth_list ms)

  type stat = { mutable sum_payoff : float;  mutable n_trials : int }

  let add_one_playout :
  type a. a game -> a move list -> stat array -> unit =
    fun game ms ts ->
    let { data = { current = p }} = game in
    let tot =
      let f tot { n_trials = n } = tot + n in
      Array.fold_left f 0 ts in
    let (i, _) =
      let rec n = Array.length ts in
      let rec loop i (i_acc,  v_acc) =
        if i >= n then (i_acc, v_acc)
        else
          let { n_trials = n_trials; sum_payoff = sum_payoff } = ts.(i) in
          if n_trials = 0 then
            loop (i + 1) (i, infinity)
          else
            let f_trials = (float_of_int n_trials) in
            let v =
              sum_payoff /. f_trials
              +. param  *. sqrt (log (float_of_int tot) /. f_trials) in
            begin
              (* Printf.printf "%f\n" v; *)
              if v > v_acc then
                loop (i + 1) (i,  v)
              else loop (i + 1) (i_acc,  v_acc)
            end in
      loop 0 (-1, neg_infinity) in
    if i < 0 then
      failwith "add_one_playout: No available moves"
    else
      let po = simple_playout p game (List.nth ms i) in
      ts.(i).sum_payoff <- ts.(i).sum_payoff +. po;
      ts.(i).n_trials <- ts.(i).n_trials + 1

  let  good_move' : type a. a game -> (a move * float) =
    fun g ->
    let ms = moves g in
    match ms with
    | [m] -> (m, nan)
    | _ ->
       let n = (List.length ms) in
       let ts =
         let f _ = { sum_payoff = 0.; n_trials = 0 } in
         Array.init n f in
       begin
         let handle_koi g i j =
           ts.(i).sum_payoff <- simple_playout g.data.current g No_koi;
           ts.(i).n_trials <- 1;
           let rec loop i =
             if i >= limit then ()
             else
               let g = random g in
               let po = simple_playout g.data.current g Koi in
               ts.(j).sum_payoff <- ts.(j).sum_payoff +. po;
               ts.(j).n_trials <- ts.(j).n_trials + 1;
               loop (i + 1) in
           loop 0 in
         match ms with
         | [Koi; No_koi] -> handle_koi g 1 0
         | [No_koi; Koi] -> handle_koi g 0 1
         | _ ->
           let rec loop i =
             if i >= limit then ()
             else
               let g' = random g in
               (* assert (ms = moves g'); *)
               (* if g'.data.yama = g.data.yama then Printf.printf "Cheating???"; *)
               add_one_playout g' ms ts;
               loop (i + 1) in
           loop 0
       end;
       let i, po_acc =
         let rec loop i (i_acc, po_acc) =
           if i >= n then i_acc, po_acc
           else
             let s = ts.(i) in
             let po_exp = (s.sum_payoff /. float_of_int s.n_trials) in
             Printf.fprintf stderr "%d: %d %f\n" i s.n_trials po_exp;
             if po_exp > po_acc then loop (i + 1) (i, po_exp)
             else loop (i + 1) (i_acc, po_acc) in
         loop 0 (-1, neg_infinity) in
       if i < 0 then
         failwith "good_move: No available moves???"
       else List.nth ms i, po_acc
  let  good_move : type a. a game -> a move =
    fun g -> fst (good_move' g)
end

module Names = struct (* Bad copy & pasting !! *)
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
  | CExist (Kara_card (m, _)) -> cat "kara" m

  let string_of_yaku y =
    let f = function
      | Gokoo -> "Gokô"
      | Sikoo -> "Honsikô"
      | Amesikoo -> "Amesikô"
      | Akatan -> "Sugawara"
      | Aotan -> "Aotan"
      | Nanatan _ -> "Syônana"
      (* | Inosikatyoo -> "Ino-sika-tyô" *)
      | Kara _ -> "Kara" in
    Printf.sprintf "%s (%d)" (f y) (util_of_yaku y)
end

(* 9 point(s) after 120 game(s). *)

(* http://www.geocities.jp/xmbwq497/gihou/hanafuda-gihou1.html *)
open MyStd
open Typerep_lib.Std

let (@) = List.rev_append

type util = int [@@deriving typerep]
type month =
  | Matu   | Ume   | Sakura
  | Huzi   | Ayame | Botan
  | Hagi   | Susuki| Kiku
  | Momizi | Yanagi| Kiri [@@deriving typerep]
type rank = One | Two | Three | Four [@@deriving typerep]
type card = month * rank [@@deriving typerep]
type yaku' = Count of util * int * (card -> bool)
type yaku = string * yaku'
type yaku_result = string * util [@@deriving typerep]
type yaku_type = Simple | Deiri
type yaku_join_type = Sum | Conditional
type rule = Till_end | Abort_when_yaku
type payoff_type = Difference | Absolute
type oni_type = Leave_rest | Take_rest
type player_id = PI | PII [@@deriving typerep]
type deal_type = No_basi | No_basanbon
type megati = Megati_none | Megati_thru of int

module type GAME = sig
  val util_of_card : card -> util
  val yaku : yaku list
  val yaku_type : yaku_type
  val yaku_join_type : yaku_join_type
  val rule : rule
  val payoff_type : payoff_type
  val oni_type : oni_type
  val is_oni : [`Te | `Mekuri | `Ba] -> card -> bool
  val n_te : int
  val n_ba : int
  val handle_sarasi : player_id -> card list -> (util * (card list))
  val deal_type : deal_type
  val bound : int option
  val megati : megati
  val remote_url : string
  module UCB1 : sig val limit : int val param : float end
end
module Make (G : GAME) = struct

  type tori = { data : card list; mutable memo : yaku_result list option }
        [@@deriving typerep]

  (* Do not want to use polymorphic eq function *)
  (* Idk why, but = emits that for rank *)
  let eq_card ((m, r) : card) ((m', r') : card) = m == m' && r == r'

  let util_of_yaku_result ((_, u) : yaku_result) = u
  let util_of_yaku_results ys =
    let f s y = s + util_of_yaku_result y in
    List.fold_left f 0 ys

  let yaku_results_of_tori' y cs =
    match y with
    | (s, Count (u, n, f)) ->
       let rec loop n' cs =
         if n' >= n then Some (s, u)
         else
           match cs with
           | [] -> None
           | c :: cs ->
              if f c then loop (n' + 1) cs else loop n' cs in
       loop 0 cs

  let yaku_results_of_tori ({ data = cs; memo } as t) =
    match memo with
    | Some ys -> ys
    | _ ->
       let f acc y =
         match yaku_results_of_tori' y cs with
         | Some x -> x :: acc
         | None -> acc in
       let ys = List.fold_left f [] G.yaku in
       t.memo <- Some ys;
       ys

  let util_of_tori0 cs = 
    let f s c = s + G.util_of_card c in
    List.fold_left f 0 cs.data

  let util_of_tori =
    match G.yaku_join_type with
    | Sum ->
       fun ?(thru = false) cs ->
        let _ = thru in
        let u = util_of_tori0 cs in
        let u' =
          let ys = yaku_results_of_tori cs in
          let f s y = s + util_of_yaku_result y in
          List.fold_left f 0 ys in
        u + u'
    | Conditional ->
       fun ?(thru = false) cs ->
       match yaku_results_of_tori cs with
       | [] when thru -> util_of_tori0 cs 
       | [] when not thru -> 0
       | ys ->
          let f s y = s + util_of_yaku_result y in
          List.fold_left f 0 ys 

  let cons (c : card) (t : tori) = { data = c :: t.data; memo = None }

  type player = { hand : card list; tori : tori; sarasi : card list }
        [@@deriving typerep]

  let other = function PI -> PII | PII -> PI

  type game' =
    { current : player_id;
      ba : card list;
      yama : card list;
      pi : player;
      pii : player } [@@deriving typerep]

  type play_t
  type awase1_t
  type awase2_t
  type winning_t
  type thru_t
  type _ game_phase =
    | Play_phase : play_t game_phase
    | Awase1_phase : card -> awase1_t game_phase (* lastly played card *)  
    | Awase2_phase : card -> awase2_t game_phase (* lastly drawn card *)
    | Winning_phase : winning_t game_phase (* won with a yaku *)
    | Thru_phase : thru_t game_phase (* run out of tehuda *)

  type 'a game =
    { phase : 'a game_phase;
      data : game' }

  (* Make sure to edit this module accordingly to the actual defns above *)
  module For_typerep = struct
    type game_phase =
       | Play_phase
       | Awase1_phase of card
       | Awase2_phase of card
       | Winning_phase
       | Thru_phase of card [@@ deriving typerep]
    type game = { phase : game_phase; data : game' } [@@ deriving typerep]
  end
  let packed_typerep_of_game = Some (Typerep.T For_typerep.typerep_of_game)
                         
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
    | Play : card -> play_t move
    | Awase1 : card -> awase1_t move
    | Awase1_nop : awase1_t move
    | Awase1_basanbon : card * card * card -> awase1_t move
    | Awase2 : card * card -> awase2_t move (* The card drawn comes first *)
    | Awase2_basanbon : card * card * card * card -> awase2_t move
    | Awase2_nop : card -> awase2_t move

  let awase ((m, r) : card) ba =
    let f acc ((m', r') as c') = if m' = m then c' :: acc else acc in 
    List.fold_left f [] ba

  (* Used partially by UI *)
  let can_be_matched ((m, _) as ba) pvar ((m', _) as c) =
    m = m' || G.is_oni `Ba ba || G.is_oni pvar c

  let moves : type a. a game -> a move list =
    function
    | { phase = Awase1_phase c; data = data } ->
       begin
         let ms =
           if G.is_oni `Te c then
             (* FIXME (?) : onihuda can for now capture one of basanbon *)
             List.map (fun c -> Awase1 c) data.ba
           else [] in
         let ms =
           let f acc c =
             if G.is_oni `Ba c then Awase1 c :: acc else acc in
           List.fold_left f ms data.ba in
         let ms = 
           match awase c data.ba with
           | [] -> ms
           | x::y::z::w::_ -> failwith "moves: too many matching cards"
           | x::y::z::[] -> Awase1_basanbon (x, y, z) :: ms 
           | x::y::[] -> Awase1 x :: Awase1 y :: ms
           | x::[] -> Awase1 x :: ms in
         match ms with
         | [] -> [Awase1_nop]
         | _ -> ms
       end
    | { phase = Awase2_phase c; data = data } -> 
       begin
         let ms =
           if G.is_oni `Mekuri c then
             (* FIXME (?) : ditto *)
             List.map (fun c' -> Awase2 (c, c')) data.ba
           else [] in
         let ms =
           let f acc c' =
             if G.is_oni `Ba c' then Awase2 (c, c') :: acc else acc in
           List.fold_left f ms data.ba in
         let ms = 
           match awase c data.ba with
           | [] -> ms
           | x::y::z::w::_ -> failwith "moves: too many matching cards"
           | x::y::z::[] -> Awase2_basanbon (c, x, y, z) :: ms
           | x::y::[] -> Awase2 (c, x) :: Awase2 (c, y) :: ms
           | x::[] -> Awase2 (c, x) :: ms
           (* | cs -> List.map (fun c' -> Awase2 (c, c')) cs *) in
         match ms with
         | [] -> [Awase2_nop c]
         | _ -> ms
       end
    | { phase = Play_phase } as g ->
       let p = player_of_game g in
       List.map (fun c -> Play c) p.hand
    | { phase = Winning_phase } -> failwith "moves: applied to a winning game"
    | { phase = Thru_phase } -> failwith "moves: applied to a thru game"

  let bound =
    match G.bound with
    | None -> fun x -> x
    | Some n -> fun (x : int) -> min x n

  let payoff_winning : winning_t game -> util * util
    = fun { data = { pi = { tori }; pii = { tori = tori' }}} ->
    (bound (util_of_tori tori), bound (util_of_tori tori'))

  let payoff_thru : thru_t game -> util * util
    = fun { data = { pi = { tori }; pii = { tori = tori' }}} ->
    let u, u' = (util_of_tori ~thru:true tori, util_of_tori ~thru:true tori') in
    let u, u' =
      match G.megati with
      | Megati_thru n ->
         if u > u' then (n, 0)
         else if u < u' then (0, n)
         else (0, 0)
      | _ -> u, u' in
    match G.payoff_type with
    | Absolute -> (bound u, bound u')
    | Difference when u > u' -> (bound (u - u'), 0)
    | _ -> (0, bound (u' - u))

  let payoff : type a. a game -> (util * util) option
    = function
    | { phase = Winning_phase } as g -> Some (payoff_winning g)
    | { phase = Thru_phase } as g -> Some (payoff_thru g)
    | _ -> None

  type egame = GExist : 'a game -> egame

  (* From containers *)
  (* monomorphic list operations *) 
  let remove_card x l =
    let rec remove' x acc l = match l with
      | [] -> List.rev acc
      | y :: tail when eq_card x y -> remove'  x acc tail
      | y :: tail -> remove'  x (y::acc) tail
    in
    remove'  x [] l
  let diff_card (xs : card list) (ys : card list) =
    let f xs y = remove_card y xs in
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
         { data with ba = diff_card data.ba cs } in
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

  let check_thru ({ phase = Play_phase; data = data } as g) =
    match data.pi.hand, data.pii.hand with
    | [], [] -> GExist { phase = Thru_phase; data = data }
    | _ -> GExist { g with data = swap' data}

  let apply_awase2 { phase = Awase2_phase c; data = data } m =
    let p = player_of_game' data in
    let help tori cs =
      let p = { p with tori = tori } in
      let data = update_current_player' data p in
      { data with ba = diff_card data.ba cs } in
    let k data =
      let p = (player_of_game' data) in
      match G.rule with
      | Till_end -> (check_thru { phase = Play_phase; data = data })
      | Abort_when_yaku ->
         let ys = yaku_results_of_tori p.tori in
         match ys with
         | [] ->
            (check_thru { phase = Play_phase; data = data })
         | _ -> GExist { phase = Winning_phase; data = data } in
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
    let p = { p with hand = remove_card c p.hand;
                     sarasi = remove_card c p.hand } in
    (* assert (List.length p.hand = l - 1); *)
    { phase = Awase1_phase c; data = update_current_player' data p }

  let apply : type a. a game -> a move -> egame
    = fun g m ->
    (* assert (List.mem m (moves g)); *)
    match g with
    | { phase = Awase1_phase _ } as g -> GExist (apply_awase1_ g m)
    | { phase = Awase2_phase _} as g -> apply_awase2 g m
    | { phase = Play_phase } as g -> GExist (apply_play_ g m)
    | { phase = Winning_phase } ->
       failwith "apply: applied to a winning state"
    | { phase = Thru_phase } -> failwith "apply: applied to a draw game"

  let cards_of_tori ({ data } : tori) = data

  let hana_karuta : card list =
    List.flatten
    @@ List.map (fun r ->
           List.map (fun m -> (Obj.magic m, r)) [0;1;2;3;4;5;6;7;8;9;10;11])
                [One;Two;Three;Four]

  let has k (hand : card list) =
    let rec loop m =
      if m >= 12 then false
      else
        if
          let f acc (m', _) = if (Obj.magic m) = m' then 1 + acc else acc in
          k <= List.fold_left f 0 hand
        then true
        else loop (1 + m) in
    loop 0

  let rec take_drop_wo k n xs0 =
    let xs, xs' = List.take_drop n xs0 in
    if has k xs then take_drop_wo k n (Random.shuffle_list xs0)
    else xs, xs'

  let take_drop_wo_si n xs0 = take_drop_wo 4 n xs0
  let take_drop_for_ba =
    match G.deal_type with
    | No_basanbon -> fun n xs0 -> take_drop_wo 3 n xs0
    | No_basi -> take_drop_wo_si

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
    let visible =
      additional @ p.hand @ g.data.ba @ cs_tori @ cs_tori' @ p'.sarasi in
    let cs = diff_card hana_karuta visible in
    let cs = Random.shuffle_list cs in
    let hand', cs = take_drop_wo_si (List.length (diff_card p'.hand p'.sarasi)) cs in (* I'm not cheating! *)
    assert (List.length cs = List.length g.data.yama);
    let hand2 = hand' @ p'.sarasi in
    assert (List.length hand2 = List.length p'.hand);
    let p' = { p' with hand = hand2 } in
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
      { data = []; memo = None } in
    let ui, si = G.handle_sarasi PI hand in
    assert (ui = 0);
    let uii, sii = G.handle_sarasi PII hand' in
    assert (uii = 0);
    let pi = { hand = hand; tori = empty; sarasi = si } in
    let pii = { hand = hand'; tori = empty; sarasi = sii } in
    let yama = Random.shuffle_list (diff_card hana_karuta (ba @ hand @ hand')) in
    let data = { pi = pi; pii = pii; ba = ba; yama = yama; current = PI } in
    { phase = Play_phase; data = data }
      
  let init_from (ba, hand) =
    let cs = diff_card hana_karuta (ba @ hand) in
    let cs = Random.shuffle_list cs in
    let hand', _ = take_drop_wo_si G.n_te cs in
    join (ba, hand) (ba, hand')

  let init () =
    let cs = Random.shuffle_list hana_karuta in
    let ba, cs = take_drop_for_ba G.n_ba cs in
    let hand, _ = take_drop_wo_si G.n_te cs in
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

  (* Used by the UI *)
  let cards_of_yaku (_, Count (_, _, g)) = List.filter g hana_karuta
              
  let play_guide (c : card) =
    let f ((s, Count (_, _, g)) as y) = if g c then Some y else None in
    let ys = List.filter_map f G.yaku in
    let f ((s, Count (u, n, g)) as y) =
      let cs = cards_of_yaku y in
      (s, u, n, cs) in
    (G.util_of_card c, [], List.map f ys)

  module MCUCB1 = struct
    let param = G.UCB1.param
    let limit = G.UCB1.limit
                  
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
      | Some (po_i, po_ii) ->
         begin
           match p with
           | PI -> float (po_i - po_ii)
           | PII -> float (po_ii - po_i)
         end
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
              let v =
                let f_trials = (float_of_int n_trials) in
                match ms with
                | [_; _] -> ~-. f_trials
                | _ ->
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
end
                                  

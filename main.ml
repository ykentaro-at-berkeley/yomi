open Mekuri
open Names
open Printf

module M = MCUCB1(struct let limit = 1_0000 let param = 50. end)

let id x = x

type t = { visible : bool; name : string;
           choose_move : 'a. 'a game -> 'a move list -> 'a move }

let human =
  { visible = true;
    name = "Human";
    choose_move  =
      fun _ ms ->
      printf "Choose an index: ";
      flush stdout;
      let n = read_int () in
      List.nth ms n }

let ai =
  { visible = false;
    name = "AlphaKoikoi";
    choose_move = fun g _ -> M.good_move g }

let basanbon p (c', c'', c''') = 
  printf
    "%s got three cards: %s, %s, and %s.\n"
    p.name
    (string_of_card c') (string_of_card c'') (string_of_card c''')

let print_tori tori = printf "Utility is %d\n" (util_of_tori tori)

let print_list cs =
  List.iter (fun c -> Printf.printf "%s\n" (string_of_card c)) cs
let rec loop_play p (g : play_t game)  =
  match payoff g with
  | Some f -> failwith "loop_play : game should have been over"
  | None ->
     printf "--------------------\nBa:\n";
     print_list g.data.ba;
     printf "\n%s's torihuda:\n" p.name;
     print_list @@ cards_of_tori (player_of_game g).tori;
     if true then 
       begin
         printf "\n%s's hand:\n" p.name;
         print_list (player_of_game g).hand;
       end;
     let ms = moves g in
     let m = p.choose_move g ms in
     match apply g m with
     | GExist ({ phase = Awase1_phase c } as g) ->
        begin
          printf "%s played %s.\n" (p.name) (string_of_card c);
          let ms = moves g in
          match ms with
          | [Awase1_nop] ->
             printf "%s is getting no card.\n" p.name;
             loop_awase1 p g Awase1_nop
          | [(Awase1_basanbon (c', c'', c''')) as m] ->
             basanbon p (c', c'', c''');
             loop_awase1 p g m
          | [(Awase1 c') as m] ->
             printf "%s is getting %s.\n" p.name (string_of_card c');
             loop_awase1 p g m
          | [(Awase1 c1); (Awase1 c2)] ->
             printf
               "There are two cards that match: %s and %s.\n"
               (string_of_card c1) (string_of_card c2);
             let (Awase1 c') as m = p.choose_move g ms in
             printf "%s is getting %s.\n" p.name (string_of_card c');
             loop_awase1 p g m
          | _ -> failwith "loop_play: kaboom!"
        end
and loop_awase1 p (g : awase1_t game) (m : awase1_t move) =
  match apply g m with
  | GExist ({ phase = Awase2_phase c; } as g) ->
     printf "%s drew %s.\n" p.name (string_of_card c);
     match moves g with
     | [Awase2_nop c] ->
        printf "%s is getting no card.\n" p.name;
        loop_awase2 p g (Awase2_nop c)
     | [(Awase2_basanbon (_, c', c'', c''')) as m] ->
        basanbon p (c', c'', c''');
        loop_awase2 p g m
     | [(Awase2 (_, c')) as m] ->
        printf "%s is getting %s.\n" p.name (string_of_card c');
        loop_awase2 p g m
     | [Awase2 (_, c1); Awase2 (_, c2)] as ms ->
        printf
          "There are two cards that match: %s and %s.\n"
          (string_of_card c1) (string_of_card c2);
        let (Awase2 (_, c')) as m = p.choose_move g ms in
        printf "%s is getting %s.\n" p.name (string_of_card c');
        loop_awase2 p g m
and loop_awase2 p (g : awase2_t game) (m : awase2_t move) =
  match apply g m with
  | GExist ({ phase = Play_phase } as g) ->
     loop_play (if p.visible then ai else human) g
  | GExist ({ phase = Koi_phase } as g) ->
     printf "%s has a (new) yaku: \n" p.name;
     print_tori (player_of_game g).tori;
     printf "%s can now choose between Koi and No_koi.\n" p.name;
     let m = p.choose_move g (moves g) in
     printf "%s chose %s.\n" p.name (match m with Koi -> "Koi" | _ -> "No_koi");
     match apply g m with
     | GExist ({ phase = Play_phase } as g) ->
        loop_play (if p.visible then ai else human) g
     | GExist ({ phase = Winning_phase } as g) ->
        print_tori (player_of_game g).tori;
        printf "%s won.\n" p.name (* No recursive call *)
     | GExist ({ phase = Draw_phase }) ->
        printf "The game ended in draw.\n"

let () =
  Random.init 424242;
  let g = init () in
  loop_play human g

open Mekuri
open Printf

module M = MCUCB1(struct let limit = 1_0000 let param = 50. end)

let (>>=) = Lwt.bind
module Html = Dom_html
module Events = Lwt_js_events

let js = Js.string
let document = Html.window##.document

let append_text e s =
  let d = Html.createDiv document in
  Dom.appendChild e d;
  d##.innerHTML := js s;
  d##scrollIntoView Js._false

let path_of_card (mon, ran) =
  let ir =
    match ran with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4 in
  Printf.sprintf "images/Hanafuda %d-%d.svg" mon ir

let card_width = 93
let card_height = 147

let string_of_card c' =
  Printf.sprintf "<img width=%d height=%d src=\"%s\"></img>"
                 card_width card_height
                 (path_of_card c')

let string_of_move : type a. a move -> string = function
  | Koi -> "Declare Koi"
  | No_koi -> "Do not declare Koi"
  | Awase1 c -> Names.string_of_card c
  | Awase2 (_, c) -> Names.string_of_card c
  | Play c -> Names.string_of_card c
  | _ -> "Unknown name"

let printf' s =
  let body =
    Js.Opt.get (document##getElementById (js "yomi"))
               (fun () -> assert false) in
  append_text body s
  
               
let printf fmt = Printf.ksprintf printf' fmt

let bank =
  let f c = (c, Html.createImg document) in
  List.map f hana_karuta
let wait_for_bank () =
  let ts =
    let f (c, i) =
      i##.src := js (path_of_card c);
      Events.load i >>= fun _ -> Lwt.return () in
    List.map f bank in
  Lwt.join ts

type t = { visible : bool; name : string;
           choose_move : 'a. 'a game -> 'a move list -> 'a move Lwt.t}

let human =
  { visible = true;
    name = "Human";
    choose_move  =
      fun _ ms ->
      let body =
        Js.Opt.get (document##getElementById (js "yomi"))
                   (fun () -> assert false) in
      printf "Choose a move: ";
      let f m =
        let o = Html.createButton document in
        Dom.appendChild body o;
        o##.innerHTML := js (string_of_move m);
        o##scrollIntoView Js._false;
        Events.click o >>= fun _ ->
        Lwt.return m  in
      Lwt.choose (List.map f ms) }

let ai =
  { visible = false;
    name = "AlphaKoikoi";
    choose_move = fun g _ -> Lwt.return (M.good_move g) }

let basanbon p (c', c'', c''') = 
  printf
    "%s got three cards: %s, %s, and %s.</br>"
    p.name
    (string_of_card c') (string_of_card c'') (string_of_card c''')

let print_tori tori = printf "Utility is %d</br>" (util_of_tori tori)

let print_list cs =
  printf "%s" (List.fold_left (^) "" (List.map string_of_card cs))

let rec loop_play p (g : play_t game)  =
  match payoff g with
  | Some f -> failwith "loop_play : game should have been over"
  | None ->
     printf "--------------------</br>Ba:";
     print_list g.data.ba;
     printf "</br>%s's torihuda:</br>" p.name;
     print_list @@ cards_of_tori (player_of_game g).tori;
     if p.visible then 
       begin
         printf "</br>%s's hand:</br>" p.name;
         print_list (player_of_game g).hand;
       end;
     let ms = moves g in
     p.choose_move g ms >>= (fun m ->
       match apply g m with
       | GExist ({ phase = Awase1_phase c } as g) ->
          begin
            printf "%s played %s.</br>" (p.name) (string_of_card c);
            let ms = moves g in
            match ms with
            | [Awase1_nop] ->
               printf "%s is getting no card.</br>" p.name;
               loop_awase1 p g Awase1_nop
            | [(Awase1_basanbon (c', c'', c''')) as m] ->
               basanbon p (c', c'', c''');
               loop_awase1 p g m
            | [(Awase1 c') as m] ->
               printf "%s is getting %s.</br>" p.name (string_of_card c');
               loop_awase1 p g m
            | [(Awase1 c1); (Awase1 c2)] ->
               printf
                 "There are two cards that match: %s and %s.</br>"
                 (string_of_card c1) (string_of_card c2);
               p.choose_move g ms >>= fun ((Awase1 c') as m) ->
               printf "%s is getting %s.</br>" p.name (string_of_card c');
               loop_awase1 p g m
            | _ -> failwith "loop_play: kaboom!"
          end)
and loop_awase1 p (g : awase1_t game) (m : awase1_t move) =
  match apply g m with
  | GExist ({ phase = Awase2_phase c; } as g) ->
     printf "%s drew %s.</br>" p.name (string_of_card c);
     match moves g with
     | [Awase2_nop c] ->
        printf "%s is getting no card.</br>" p.name;
        loop_awase2 p g (Awase2_nop c)
     | [(Awase2_basanbon (_, c', c'', c''')) as m] ->
        basanbon p (c', c'', c''');
        loop_awase2 p g m
     | [(Awase2 (_, c')) as m] ->
        printf "%s is getting %s.</br>" p.name (string_of_card c');
        loop_awase2 p g m
     | [Awase2 (_, c1); Awase2 (_, c2)] as ms ->
        printf
          "There are two cards that match: %s and %s.</br>"
          (string_of_card c1) (string_of_card c2);
        p.choose_move g ms >>= fun ((Awase2 (_, c')) as m) ->
        printf "%s is getting %s.</br>" p.name (string_of_card c');
        loop_awase2 p g m
and loop_awase2 p (g : awase2_t game) (m : awase2_t move) =
  match apply g m with
  | GExist ({ phase = Play_phase } as g) ->
     loop_play (if p.visible then ai else human) g
  | GExist ({ phase = Koi_phase } as g) ->
     printf "%s has a (new) yaku: </br>" p.name;
     print_tori (player_of_game g).tori;
     printf "%s can now choose between Koi and No_koi.</br>" p.name;
     p.choose_move g (moves g) >>= (fun m ->
       printf "%s chose %s.</br>" p.name (match m with Koi -> "Koi" | _ -> "No_koi");
       match apply g m with
       | GExist ({ phase = Play_phase } as g) ->
          loop_play (if p.visible then ai else human) g
       | GExist ({ phase = Winning_phase } as g) ->
          print_tori (player_of_game g).tori;
          printf "%s won.</br>" p.name (* No recursive call *);
          Lwt.return ();
       | GExist ({ phase = Draw_phase }) ->
          printf "The game ended in draw.</br>";
          Lwt.return ())

let start () =
  Random.self_init ();
  let g = init () in
  loop_play human g

let _ =
  Html.window##.onload := Html.handler (fun _ -> ignore (start ()); Js._false)

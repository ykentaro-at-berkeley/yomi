open Mekuri
open Printf
open Names

module M = MCUCB1(struct let limit = 5000 let param = 50. end)

let (>>=) = Lwt.bind
module Html = Dom_html
module Events = Lwt_js_events

let js = Js.string
let document = Html.window##.document

let hash x = Hashtbl.hash_param 20 100 x

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

let card_width = 62
let card_height = 98

let string_of_card c' =
  let h = hash c' in
  Printf.sprintf "<img id='img%d' width=%d height=%d src=\"%s\"></img>"
                 h
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

let worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t
  = Worker.create "sigotonin0.js"
(* let bank = *)
(*   let f c = (c, Html.createImg document) in *)
(*   List.map f hana_karuta *)
(* let wait_for_bank () = *)
(*   let ts = *)
(*     let f (c, i) = *)
(*       i##.src := js (path_of_card c); *)
(*       Events.load i >>= fun _ -> Lwt.return () in *)
(*     List.map f bank in *)
(*   Lwt.join ts *)

type t = { visible : bool; name : string;
           choose_move : 'a. 'a game -> 'a move list -> 'a move Lwt.t}

let catch_raise t =
  Lwt.catch (fun () -> t)
            (fun e -> Printf.printf "%s\n" @@ Printexc.to_string e; raise e)

let human_choose_move : type a. a game -> a move list -> a move Lwt.t =
  (fun _ ms ->
    let body =
      Js.Opt.get (document##getElementById (js "yomi"))
                 (fun () -> assert false) in
    printf "Choose a move: ";
    match ms with
    | (Play _) ::_ ->
       let cs = List.map (fun (Play c) -> c) ms in
       let h = hash cs in
       let id = Printf.sprintf "hash%d" h in
       let div = Js.Opt.get
                   (document##getElementById (js id))
                   (fun () ->
                     failwith "human.choose_move: can't get the div") in
       let f (Play c as m)  =
         let i =
           let id = Printf.sprintf "#img%d" (hash c) in
           Js.Opt.get (div##querySelector (js id))
                      (fun () ->
                        failwith "human.choose_move: can't get the img") in
         catch_raise @@ Events.click i >>= fun _ ->
         Lwt.return m in
       Lwt.choose (List.map f ms)
    | _ ->
       let f m =
         let o = Html.createButton document in
         Dom.appendChild body o;
         o##.innerHTML := js (string_of_move m);
         o##scrollIntoView Js._false;
         catch_raise @@ Events.click o >>= (fun _ ->
         Lwt.return m ) in
       Lwt.choose (List.map f ms))

let human =
  { visible = true;
    name = "Human";
    choose_move   = human_choose_move }

let rec ai =
  { visible = false;
    name = "Computer";
    choose_move = fun g _ -> Lwt.return (M.good_move g)
                  (* printf "%s is thinking..." ai.name; *)
                  (* worker##postMessage (Json.output g); *)
                  (* let ev = Html.Event.make "message" in *)
                  (* Events.make_event ev worker >>= *)
                  (*   fun e -> *)
                  (*   let move = Json.unsafe_input e##.data in *)
                  (*   Lwt.return move *)
  }

let basanbon p (c', c'', c''') = 
  printf
    "%s got three cards: %s, %s, and %s.</br>"
    p.name
    (string_of_card c') (string_of_card c'') (string_of_card c''')

let print_list rightleader cs =
  let h = hash cs in
  let content = (List.fold_left (^) "" (List.map string_of_card cs)) in
  match  rightleader with
  | Some s -> 
     printf "<div style='float:right' id='hash%d'>%s</br>%s</div>" h s content
  | None -> printf "<div id = 'hash%d'>%s</div>" h content

let print_tori tori =
  List.iter (fun y -> printf "%s" (string_of_yaku y)) (yaku_of_tori tori);
  print_list None (cards_of_tori tori)

let rec loop_play p (g : play_t game)  =
  match payoff g with
  | Some f -> failwith "loop_play : game should have been over"
  | None ->
     printf "<hr>";
     let () =
       let s = 
         Printf.sprintf "%s's torihuda:" p.name in
       print_list (Some s) (cards_of_tori (player_of_game g).tori) in
     printf "Ba:</br>";
     print_list None g.data.ba;
     if p.visible then 
       begin
         printf "</br>%s's hand:</br>" p.name;
         print_list None (player_of_game g).hand;
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
  let k_draw () =
    printf "The game ended in draw.</br>";
    Lwt.return () in
  match apply g m with
  | GExist ({ phase = Draw_phase }) -> k_draw ()
  | GExist ({ phase = Play_phase } as g) ->
     loop_play (if p.visible then ai else human) g
  | GExist ({ phase = Koi_phase } as g) ->
     printf "%s has a (new) yaku: </br>" p.name;
     print_tori (player_of_game g).tori;
     printf "%s can now declare koi.</br>" p.name;
     p.choose_move g (moves g) >>= (fun m ->
       printf "%s chose %s.</br>" p.name (match m with Koi -> "Koi" | _ -> "No_koi");
       match apply g m with
       | GExist ({ phase = Play_phase } as g) ->
          loop_play (if p.visible then ai else human) g
       | GExist ({ phase = Winning_phase } as g) ->
          let tori = (player_of_game g).tori in
          print_tori tori;
          printf "%s won with payoff %d.</br>" p.name (util_of_tori tori);
          Lwt.return ()
       | GExist ({ phase = Draw_phase }) -> k_draw ())

let start () =
  Random.self_init ();
  let g = init () in
  loop_play human g

let _ =
  Html.window##.onload := Html.handler (fun _ -> ignore (start ()); Js._false)

open Edsl

module Make (G : Edsl.GAME) = struct
  module M = Edsl.Make(G)
  open M

  let (>>=) = Lwt.bind
  module Html = Dom_html
  module Events = Lwt_js_events

  let js = Js.string
  let js_sprintf fmt = Printf.kprintf js fmt
                                      
  let document = Html.window##.document

  let catch_raise t =
    Lwt.catch (fun () -> t)
              (fun e -> Printf.printf "%s\n" @@ Printexc.to_string e; raise e)

  let append_text e s =
    let d = Html.createDiv document in
    Dom.appendChild e d;
    d##.innerHTML := js s

  let path_of_card ((mon, ran) : card) =
    let ir =
      match ran with
      | One -> 1
      | Two -> 2
      | Three -> 3
      | Four -> 4 in
    Printf.sprintf "images/Hanafuda %d-%d.svg" (1 + Obj.magic mon) ir

  let card_width = 62
  let card_height = 98

  let dialog f =
    let b = Html.createDiv document in
    b##.style##.cssText :=
      js "position: fixed;\
          z-index: 1000;\
          left: 0;\
          top: 0;\
          width: 100%;\
          height: 100%;\
          overflow: auto;\
          background-color: rgba(0,0,0,0.3);";
    let c = Html.createDiv document in
    Dom.appendChild b c;
    c##.style##.cssText :=
      js "background-color: rgba(255,255,255,0.85);\
          color: black;\
          margin: 15% auto;\
          padding: 20px;\
          border: 1px solid #888;\
          width: 80%";
    (b, c)

  module Drawer = struct
    let y_ai = 0
    let y_ba = card_height + card_height/2
    let y_human = 4*card_height + card_height/2
    let x_tori = card_width*11 + card_width/2
    let gap = 7

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
    let div = Html.createDiv document

    type repr = card * Html.canvasElement Js.t
    let cards : repr list ref = ref []

    let mes = Html.createDiv document
    let ds = Html.createDiv document
    let init s body =
      Dom.appendChild body ds;
      Dom.appendChild body mes;
      ds##.innerHTML := js s;
      mes##.innerHTML := js "";
      cards := [];
      div##.innerHTML := js "";
      div##.style##.width := js_sprintf "%dpx" (card_width * 12);
      div##.style##.height := js_sprintf "%dpx" (card_width * 8);
      div##.style##.position := js "relative";
      Dom.appendChild body div

    (* let compare r r' = Pervasives.compare r.z r'.z *)

    (* let draw () = *)
    (*   let ctx = canvas##getContext Html._2d_ in *)
    (*   let cards = List.sort compare !cards in *)
    (*   ctx##clearRect 0.0 0.0 (float canvas##.width) (float canvas##.height); *)
    (*   let f r = *)
    (*     ctx##.fillStyle := js "white"; *)
    (*     ctx##fillRect (float r.x) (float r.y) *)
    (*                   (float card_width) (float card_height); *)
    (*     ctx##drawImage_withSize r.image *)
    (*                             (float r.x) (float r.y) *)
    (*                             (float card_width) (float card_height) in *)
    (*   List.iter f cards *)

    let get_repr (c : card) =
      try (c, List.assoc c !cards)
      with
      | Not_found ->
         Printf.printf "get_repr: nonexistent repr\n";
         raise Not_found

    let set_visibility' canv (b : bool) =
      canv##.style##.visibility := js (if b then "visible" else "hidden")

    let set_visibility c b =
      let _, canv = get_repr c in set_visibility' canv b

    let set_coord' canv x y = 
      canv##.style##.left := js_sprintf "%dpx" x;
      canv##.style##.top := js_sprintf "%dpx" y

    let set_coord c x y =
      let _, canv = get_repr c in set_coord' canv x y

    let card_border = "2px outset black"

    let make_repr_internal c =
      let canv = Html.createCanvas document in
      canv##.width := card_width;
      canv##.height := card_height;
      let card_width, card_height =
        float card_width, float card_height in
      let ctx = canv##getContext Html._2d_ in
      ctx##.fillStyle := js "white";
      ctx##fillRect 0. 0. card_width card_height;
      ctx##drawImage_withSize (List.assoc c bank)
                              0. 0.
                              card_width card_height;
      canv##.style##.border := js card_border;
      canv

    let make_repr (c : card) =
      try (c, List.assoc c !cards) with (* *)
      | Not_found ->
         let canv = make_repr_internal c in
         let r = (c, canv) in
         cards := r::!cards;
         Dom.appendChild div canv;
         canv##.style##.position := js "absolute";
         set_visibility' canv true;
         r


    (* 16 should be enough for 10/8 or 8/8 *)
    let ba : card option array = Array.make 16 None

    let unplace (c : card) =
      let n = Array.length ba in
      let rec loop i =
        if i >= n then ()
        else
          match ba.(i) with
          | Some c' when eq_card c c' ->
             ba.(i) <- None
          | _ -> loop (i + 1) in
      loop 0

    let delete_repr (c : card) =
      let (_, canv) = get_repr c in
      Dom.removeChild div canv;
      cards := List.remove_assoc c !cards;
      unplace c

    let coord_of_idx i = 
      let coord_of_idx i =
        match i mod 2 with
        | 0 -> ((i/2) * (card_width + gap), y_ba)
        | _ -> ((i/2) * (card_width + gap), y_ba + card_height + gap) in
      coord_of_idx ((Array.length ba) - 1 - i)

    let init_ba (cs : card list) =
      Array.fill ba 0 (Array.length ba) None;
      let f i c =
        ignore (make_repr c);
        ba.(i) <- Some c;
        set_visibility c true;
        let x, y = coord_of_idx i in
        set_coord c x y;
        i - 1 in
      ignore (List.fold_left f (Array.length ba - 1) cs)

    (* (None, (x, y)) if placed; *)
    (* (Some r, (x, y))  if there is a card of the same month on the ba *)
    let place pvar ((m, _) as c : card)  =
      let n = Array.length ba in
      let rec loop i i0 =
        if i >= n then (* Must place it *)
          begin
            ba.(i0) <- Some c;
            (None, coord_of_idx i0)
          end
        else 
          match ba.(i) with
          | Some c' when can_be_matched c' pvar c ->
             (ba.(i), coord_of_idx i)
          | Some _ -> loop (i + 1) i0
          | None -> loop (i + 1) i in
      loop 0 (~-1)

    let y_of_polyvar (y : [`AI | `Human]) =
      match y with
      | `AI -> y_ai
      | `Human -> y_human

    let make_place' y (cs : card list) =
      let rs = List.map (fun c -> make_repr c) cs in
      let f x (_, c) = set_coord' c x (y_of_polyvar y); x + card_width + gap in
      ignore (List.fold_left f 0 rs);
      List.iter (fun (_, c) -> set_visibility' c true) rs

    let compare' c c' =
      let u, u' = G.util_of_card c, G.util_of_card c' in
      if u < u' then 1
      else if u > u' then -1
      else compare c c'

    let make_place y (cs : card list) =
      make_place' y @@ List.sort compare cs

    let set_z' canv z = canv##.style##.zIndex := js_sprintf "%d" z
    let set_z c z =
      let _, canv = get_repr c in set_z' canv z

    let append_cards e cs =
      let f c =
        let canv = make_repr_internal c in
        Dom.appendChild e canv in
      List.iter f cs

    let show_tori_dialog (cs : card list ) =
      let body =
        Js.Opt.get (document##getElementById (js "yomi"))
                   (fun () -> assert false) in
      let b, cont = dialog () in
      Dom.appendChild body b;
      append_cards cont cs;
      (catch_raise @@ Events.click b) >>= (fun _ ->
        Dom.removeChild body b;
        Lwt.return ())

    let align_tori' y (cs : card list) =
      let inner =
        Js.Optdef.get Html.window##.innerWidth (fun () -> assert false) in
      let width = inner - x_tori - 10 in (* To cope with fp errors *)
      let dx = (float width) /. (float @@ List.length cs) in
      let dx = min dx (float (card_width + gap)) in
      let f (x, z) c =
        set_coord c (int_of_float x) y;
        set_z c z;
        let _, canv = get_repr c in
        canv##.onclick :=
          Html.handler (fun _ -> ignore (show_tori_dialog cs); Js._true);
        (x +. dx, z - 1) in
      ignore (List.fold_left f (float x_tori, 1 + List.length cs) cs)

    let align_tori (y : [`AI | `Human]) cs =
      align_tori' (y_of_polyvar y) (List.sort compare' cs)

    let reveal_card pvar (c : card) =
      ignore (make_repr c);
      match place pvar c with
      | None, (x, y) ->
         set_coord c x y
      | Some c', (x, y) ->
         set_z c 1;
         set_coord c (x + 20) (y + 20)

    let set_attention' canv b =
      if b then
        canv##.style##.border := js "3px solid cyan"
      else
        canv##.style##.border := js card_border

    let set_attention c b =
      let _, canv = get_repr c in set_attention' canv b

    let message s =
      mes##.innerHTML := js s
  end

  module Sound = struct
    let move = Html.createAudio document

    let init () =
      let body =
        Js.Opt.get (document##getElementById (js "yomi"))
                   (fun () -> assert false) in
      move##.autoplay := Js._false;
      move##.src := js SoundData.move;
      move##.volume := 1.0;
      move##load;
      Dom.appendChild body move
                      
    let play () =
      move##.currentTime := 0.0;
      move##play
  end

  type t = { visible : bool; name : string;
             choose_move : 'a. 'a game -> 'a move list -> 'a move Lwt.t}

  let choose_card cs =
    List.iter (fun c -> Drawer.set_attention c true) cs;
    let ts = 
      let f c =
        let _, canv = Drawer.get_repr c in
        Events.click canv >>= fun _ ->
        List.iter (fun c -> Drawer.set_attention c false) cs;
        Lwt.return c in
      List.map f cs in
    Lwt.pick ts

  let create_phony_a s =
    let a = Html.createA document in
    a##.href := js "javascript:;";
    a##.innerHTML := js s;
    a

  let create_gap () =
    let g = document##createTextNode (js " ") in
    g

  let basanbon _ (c', c'', c''') =
    Drawer.unplace c';
    Drawer.unplace c'';
    Drawer.unplace c'''

  let human_choose_move : type a. a game -> a move list -> a move Lwt.t =
    (fun g ms ->
      match g with
      | { phase = Play_phase } ->
         let cs = List.map (fun (Play c) -> c) ms in
         let ts =
           let f c =
             let (_, r) = Drawer.get_repr c in
             (catch_raise @@ Events.click r) >>= fun _ -> Lwt.return (Play c) in
           List.map f cs in
         Lwt.pick ts
      | { phase = Awase1_phase _ } ->
         (catch_raise @@ choose_card (List.map (fun (Awase1 c) -> c) ms))
         >>= (fun c -> Lwt.return (Awase1 c))
      | { phase = Awase2_phase c0 } ->
         (catch_raise @@ choose_card (List.map (fun (Awase2 (_, c)) -> c) ms))
         >>= (fun c -> Lwt.return (Awase2 (c0, c))))

  let human =
    { visible = true;
      name = "Human";
      choose_move   = human_choose_move }

  let make_ai ?(visible = false) name routine =
    (* let worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t *)
    (*   = Worker.create routine in *)
    let module M = MCUCB1 (G.UCB1) in
    let rec ai = 
      { visible;
        name = name;
        choose_move = fun g _ ->
                      Lwt_js.sleep 0.5 >>= fun () ->
                      Lwt.return (M.good_move g)
                      (* Drawer.message *)
                      (* @@ Printf.sprintf "%s is thinking..." ai.name; *)
                      (* worker##postMessage (Json.output g); *)
                      (* let ev = Html.Event.make "message" in *)
                      (* Events.make_event ev worker >>= *)
                      (*   fun e -> *)
                      (*   let move = Json.unsafe_input e##.data in *)
                      (*   Drawer.message ""; *)
                      (*   Lwt.return move *) } in
    ai

  let ai = make_ai "Computer" "hoge.js"

  let rec loop_play p (g : play_t game)  =
    if p.visible then
      Drawer.make_place `Human (player_of_game g).hand
    else Drawer.make_place `Human (player_of_game (swap g)).hand;
    match payoff g with
    | Some f -> failwith "loop_play : game should have been over"
    | None ->
       let ms = moves g in
       (catch_raise @@ p.choose_move g ms) >>= (fun m ->
         match apply g m with
         | GExist ({ phase = Awase1_phase c } as g) ->
            begin
              Drawer.reveal_card `Te c;
              Sound.play ();
              let ms = moves g in
              match ms with
              | [Awase1_nop] ->
                 (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
                 loop_awase1 p g Awase1_nop
              | [(Awase1_basanbon (c', c'', c''')) as m] ->
                 (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
                 basanbon p (c', c'', c''');
                 loop_awase1 p g m
              | [(Awase1 c') as m] ->
                 (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
                 Drawer.unplace c';
                 loop_awase1 p g m
              | ms ->
                 p.choose_move g ms >>= fun ((Awase1 c') as m) ->
                 Drawer.unplace c';
                 loop_awase1 p g m
            end)
  and loop_awase1 p (g : awase1_t game) (m : awase1_t move) =
    match apply g m with
    | GExist ({ phase = Awase2_phase c; } as g) ->
       Drawer.align_tori (if p.visible then `Human else `AI )
                         (cards_of_tori (player_of_game g).tori);
       (catch_raise @@ Lwt_js.sleep 0.3) >>= fun () ->
       Drawer.reveal_card `Mekuri c;
       Sound.play ();
       match moves g with
       | [Awase2_nop c] ->
          (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
          loop_awase2 p g (Awase2_nop c)
       | [(Awase2_basanbon (_, c', c'', c''')) as m] ->
          (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
          basanbon p (c', c'', c''');
          loop_awase2 p g m
       | [(Awase2 (_, c')) as m] ->
          (catch_raise @@ Lwt_js.sleep 0.5) >>= fun () ->
          Drawer.unplace c';
         loop_awase2 p g m
       |  ms ->
          p.choose_move g ms >>= fun ((Awase2 (_, c')) as m) ->
          Drawer.unplace c';
          loop_awase2 p g m
  and loop_awase2 p (g : awase2_t game) (m : awase2_t move) =
    let body =
      Js.Opt.get (document##getElementById (js "yomi"))
                 (fun () -> assert false) in
    let g = apply g m in
    begin
      let k p''' =
        Drawer.align_tori (if p.visible then `Human else `AI)
                          (cards_of_tori p'''.tori) in
      match g with
      | GExist { phase = Winning_phase; data } -> k (player_of_game' data)
      | GExist { phase = Thru_phase; data } -> k (player_of_game' data)
      | GExist { data = data } -> (* player has already switched *)
         Drawer.align_tori (if p.visible then `Human else `AI)
                           (cards_of_tori (player_of_game' (swap' data)).tori)
    end;
    match g with
    | GExist ({ phase = Thru_phase } as g) ->
       let Some (ui, uii) = payoff g in
       let b, c = dialog () in
       Dom.appendChild body b;
       append_text c "The game ended.\n";
       append_text c (Printf.sprintf "First player's payoff: %d" ui);
       append_text c (Printf.sprintf "Second player's payoff: %d" uii);
       Events.click b >>= fun _ ->
       Dom.removeChild body b;
       Lwt.return (ui, uii)
    | GExist ({ phase = Winning_phase } as g) ->
       let Some (ui, uii)  = payoff g in
       let b, c = dialog () in
       Dom.appendChild body b;
       append_text c (Printf.sprintf
                        "%s won with payoff %d:\n" p.name
                        (util_of_yaku_results
                           (yaku_results_of_tori (player_of_game g).tori)));
       let f (s, u) =
         append_text c (Printf.sprintf "%s (%d)\n" s u) in
       List.iter f (yaku_results_of_tori (player_of_game g).tori);
       Events.click b >>= fun _ ->
       Dom.removeChild body b;
       Lwt.return (ui, uii)
    | GExist ({ phase = Play_phase } as g) ->
       loop_play (if p.visible then ai else human) g

  let start () =
    Random.self_init ();
    let body =
      Js.Opt.get (document##getElementById (js "yomi"))
                 (fun () -> assert false) in
    body##.style##.cssText := js "background: #5F3E35; color: white";
    let t = catch_raise @@ Drawer.wait_for_bank () in
    (* let b0 = Html.createButton document in *)
    (* Dom.appendChild body b0; *)
    (* b0##.innerHTML := js "Easy AI"; *)
    (* let t0 = *)
    (*   (catch_raise @@ Events.click b0) >>= fun _ -> *)
    (*   Lwt.return "sigotoninEasy.js" in *)
    (* let b1 = Html.createButton document in *)
    (* Dom.appendChild body b1; *)
    (* b1##.innerHTML := js "Standard AI"; *)
    (* let t1 =  *)
    (*   (catch_raise @@ Events.click b1) >>= fun _ -> *)
    (*   Lwt.return "sigotoninStd.js" in *)
    (* Lwt.pick [t0; t1] >>= fun routine -> *)
    body##.innerHTML := js "Loading images...";
    t >>= (fun () ->
      body##.innerHTML := js "";
      let rec loop b n acc =
        let s =
          Printf.sprintf "You have won %d point(s) after %d game(s).  \
                          You are the %s player in this round."
                         acc n (if b then "second" else "first") in
        Drawer.init s body;
        let g = init () in
        (* Drawer.make_place_human (if b then g.data.pii else g.data.pi).hand; *)
        Drawer.init_ba g.data.ba;
        Sound.init ();
        let ai = make_ai "Computer" "routine" in
        let pi = if b then ai else human in
        begin
          if b then Drawer.make_place `AI g.data.pi.sarasi
          else Drawer.make_place `AI g.data.pii.sarasi
        end;
        loop_play pi g >>= fun (ui, uii) ->
        loop (not b) (n + 1) (acc + if b then uii - ui else ui - uii) in
      loop false 0 0)
end
                                

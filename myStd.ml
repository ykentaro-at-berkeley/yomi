module List = struct
  include List

  (* From containers *)
  (* max depth for direct recursion *)
  let direct_depth_default_ = 1000
  let mem ?(eq=(=)) x l =
    let rec search eq x l = match l with
      | [] -> false
      | y::l' -> eq x y || search eq x l'
    in search eq x l

  let add_nodup ?(eq=(=)) x l =
    if mem ~eq x l then l else x::l

  let remove_one ?(eq=(=)) x l =
    let rec remove_one ~eq x acc l = match l with
      | [] -> assert false
      | y :: tl when eq x y -> List.rev_append acc tl
      | y :: tl -> remove_one ~eq x (y::acc) tl
    in
    if mem ~eq x l then remove_one ~eq x [] l else l
  let remove ?(eq=(=)) ~x l =
    let rec remove' eq x acc l = match l with
      | [] -> List.rev acc
      | y :: tail when eq x y -> remove' eq x acc tail
      | y :: tail -> remove' eq x (y::acc) tail
    in
    remove' eq x [] l
  let remove_at_idx i l0 =
    let rec aux l acc i = match l with
      | [] -> l0
      | _::l' when i=0 -> List.rev_append acc l'
      | y::l' ->
         aux l' (y::acc) (i-1)
    in
    aux l0 [] i
  let take n l =
    let rec direct i n l = match l with
      | [] -> []
      | _ when i=0 -> safe n [] l
      | x::l' ->
         if n > 0
         then x :: direct (i-1) (n-1) l'
         else []
    and safe n acc l = match l with
      | [] -> List.rev acc
      | _ when n=0 -> List.rev acc
      | x::l' -> safe (n-1) (x::acc) l'
    in
    direct direct_depth_default_ n l
  let rec drop n l = match l with
    | [] -> []
    | _ when n=0 -> l
    | _::l' -> drop (n-1) l'
  let take_drop n l = take n l, drop n l
                                     
  (* my work *)
  let diff ?(eq=(=)) xs ys =
    let f xs y = remove ~eq:eq ~x:y xs in
    List.fold_left f xs ys
end
module Random = struct
  include Random

  (* mine *)             
  let randomth_list l =
    let n = List.length l in
    List.nth l (Random.int n)

  let rec shuffle_list = function
    | [] -> []
    | xs ->
       let l = List.length xs in
       let n = (Random.int l) in 
       let x = List.nth xs n in
       let xs = List.remove_at_idx n xs in
       x :: shuffle_list xs
end
                  

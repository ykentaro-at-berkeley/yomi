open Netcgi
open Unmagic
open Typerep_lib.Std

module type S = sig
  type 'a game
  type 'a move
  module MCUCB1 : sig
    val good_move : 'a game -> 'a move
  end
  val packed_typerep_of_game : Typerep.packed option
end

module Make (M : S) = struct
  let main (cgi:cgi) =
    let body = cgi#argument_value "BODY" in

    let g = Marshal.from_string body 0 in
    begin
      match M.packed_typerep_of_game with
      | Some (Typerep.T ty) ->
         Unmagic.tag_check ~sharing:false ty (Obj.repr g)
      | _ -> ()
    end;
    let m = M.MCUCB1.good_move g in
    let bin = Marshal.(to_bytes m [Compat_32]) in
    cgi#set_header
      ~cache:`No_cache
      ~content_type:"application/octet-stream"
      ~content_length:(Bytes.length bin) ();
    cgi#out_channel#output_bytes bin;
    cgi#out_channel#commit_work ()

  let () =
    let config = { default_config with
                   default_exn_handler = false;
                   permitted_input_content_types = ["application/octet-stream"]; } in
    Netcgi_cgi.run ~config:config main
end

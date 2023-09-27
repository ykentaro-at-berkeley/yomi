open Js_of_ocaml
open Js_of_ocaml_lwt

let (>>=) = Lwt.bind

let blob_of_bytes s =
  let n = Bytes.length s in
  let a = new%js Typed_array.uint8Array n in
  for i = 0 to n-1 do
    Typed_array.set a i (int_of_char (Bytes.get s i))
  done;
  File.blob_from_any [`arrayBufferView (a :> Typed_array.arrayBufferView Js.t)]

let bytes_of_arrayBuffer b =
  let a = new%js Typed_array.uint8Array_fromBuffer b in
  let n = a##.length in
  let s = Bytes.create n in
  for i = 0 to n-1 do
    Bytes.set s i (char_of_int (Typed_array.unsafe_get a i))
  done;
  s

let sleep_at_least dur t =
  let t_wait = Lwt_js.sleep dur in
  t >>= fun x ->
  t_wait >>= fun () ->
  Lwt.return x

let perform url g =
  let bin = Marshal.to_bytes g [] in
  let open XmlHttpRequest in
  let t =
    perform_raw
      ~content_type:"application/octet-stream"
      ~contents:(`Blob (blob_of_bytes bin))
      ~override_method:`POST
      ~response_type:XmlHttpRequest.ArrayBuffer
      url in
  sleep_at_least 1.0 t >>= fun f ->
  let content = Js.Opt.get f.content (fun () -> assert false) in
  let bin = bytes_of_arrayBuffer content in
  Lwt.return (Marshal.from_bytes bin 0)

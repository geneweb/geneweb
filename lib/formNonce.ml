let file bpath = Filename.remove_extension bpath ^ ".nonces"

(* A form is posted again within minutes; a record is kept two days. *)
let max_age = 2. *. 86400.

(* Beyond this size, the old records are dropped from the file. *)
let max_size = 1024 * 1024

let fresh () =
  let st = Random.State.make_self_init () in
  String.init 32 (fun _ -> "0123456789abcdef".[Random.State.int st 16])

let fingerprint env =
  let b = Buffer.create 1024 in
  List.iter
    (fun (k, (v : Adef.encoded_string)) ->
      if k <> "nonce" then (
        Buffer.add_string b k;
        Buffer.add_char b '=';
        Buffer.add_string b (v :> string);
        Buffer.add_char b '&'))
    env;
  Digest.to_hex (Digest.string (Buffer.contents b))

(* Lines "time nonce fingerprint". *)
let read bpath =
  match Secure.with_open_in_text (file bpath) In_channel.input_all with
  | s ->
      List.filter_map
        (fun l ->
          match String.split_on_char ' ' l with
          | [ t; n; f ] -> (
              match float_of_string_opt t with
              | Some t -> Some (t, n, f)
              | None -> None)
          | _ -> None)
        (String.split_on_char '\n' s)
  | exception Sys_error _ -> []

let saved bpath ~nonce ~fingerprint =
  List.exists (fun (_, n, f) -> n = nonce && f = fingerprint) (read bpath)

let line t nonce fingerprint = Printf.sprintf "%.0f %s %s\n" t nonce fingerprint

let record bpath ~nonce ~fingerprint =
  let fname = file bpath in
  let now = Unix.time () in
  try
    let size =
      try (Unix.stat fname).Unix.st_size with Unix.Unix_error _ -> 0
    in
    if size < max_size then
      Secure.with_open_out_gen
        [ Open_wronly; Open_append; Open_creat; Open_text ] 0o600 fname
        (fun oc -> output_string oc (line now nonce fingerprint))
    else
      let kept =
        List.filter (fun (t, _, _) -> now -. t < max_age) (read bpath)
      in
      let tmp = fname ^ ".tmp" in
      Secure.with_open_out_gen
        [ Open_wronly; Open_trunc; Open_creat; Open_text ] 0o600 tmp (fun oc ->
          List.iter (fun (t, n, f) -> output_string oc (line t n f)) kept;
          output_string oc (line now nonce fingerprint));
      Sys.rename tmp fname
  with Sys_error _ -> ()

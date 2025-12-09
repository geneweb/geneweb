let gunzip_file path =
  let ic = Stdlib.open_in_bin path in
  Fun.protect ~finally:(fun () -> Stdlib.close_in_noerr ic) @@ fun () ->
  let file_len = in_channel_length ic in
  let compressed = really_input_string ic file_len in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create (file_len * 4) in
  let p = ref 0 in
  let refill buf =
    let len = min (String.length compressed - !p) (De.bigstring_length buf) in
    Bigstringaf.blit_from_string compressed ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    Buffer.add_string r (Bigstringaf.substring buf ~off:0 ~len)
  in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ -> Buffer.contents r
  | Error (`Msg err) -> failwith ("gzip error: " ^ err)

let gzip_string ?(level = 6) input =
  let input_len = String.length input in
  if input_len = 0 then ""
  else
    let i = De.bigstring_create De.io_buffer_size in
    let o = De.bigstring_create De.io_buffer_size in
    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x1000 in
    let r = Buffer.create (input_len / 2) in
    let p = ref 0 in
    let refill buf =
      let len = min (input_len - !p) (De.bigstring_length buf) in
      Bigstringaf.blit_from_string input ~src_off:!p buf ~dst_off:0 ~len;
      p := !p + len;
      len
    in
    let flush buf len =
      Buffer.add_string r (Bigstringaf.substring buf ~off:0 ~len)
    in
    let time () = Int32.of_float (Unix.gettimeofday ()) in
    let cfg = Gz.Higher.configuration Gz.Unix time in
    Gz.Higher.compress ~level ~w ~q ~refill ~flush () cfg i o;
    Buffer.contents r

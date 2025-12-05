let gunzip_file path =
  let ic = Stdlib.open_in_bin path in
  Fun.protect ~finally:(fun () -> Stdlib.close_in_noerr ic) @@ fun () ->
  let file_len = in_channel_length ic in
  let compressed = Bigstringaf.create file_len in
  let tmp = Bytes.create (min file_len De.io_buffer_size) in
  let rec read_all pos =
    if pos < file_len then begin
      let n = Stdlib.input ic tmp 0 (min (Bytes.length tmp) (file_len - pos)) in
      if n > 0 then begin
        Bigstringaf.blit_from_bytes tmp ~src_off:0 compressed ~dst_off:pos
          ~len:n;
        read_all (pos + n)
      end
    end
  in
  read_all 0;
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let chunks = ref [] in
  let total_len = ref 0 in
  let p = ref 0 in
  let refill buf =
    let len =
      min (Bigstringaf.length compressed - !p) (De.bigstring_length buf)
    in
    Bigstringaf.blit compressed ~src_off:!p buf ~dst_off:0 ~len;
    p := !p + len;
    len
  in
  let flush buf len =
    let chunk = Bigstringaf.copy buf ~off:0 ~len in
    chunks := chunk :: !chunks;
    total_len := !total_len + len
  in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ ->
      let result = Bigstringaf.create !total_len in
      let _ =
        List.fold_left
          (fun pos chunk ->
            let len = Bigstringaf.length chunk in
            Bigstringaf.blit chunk ~src_off:0 result ~dst_off:pos ~len;
            pos + len)
          0 (List.rev !chunks)
      in
      result
  | Error (`Msg err) -> failwith ("gunzip error: " ^ err)

let gzip_string ?(level = 6) input =
  let input_len = String.length input in
  if input_len = 0 then ""
  else
    let i = De.bigstring_create De.io_buffer_size in
    let o = De.bigstring_create De.io_buffer_size in
    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x8000 in
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

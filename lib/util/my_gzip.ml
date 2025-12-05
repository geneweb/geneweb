type in_channel = {
  ic : Stdlib.in_channel;
  mutable decoder : Gz.Inf.decoder;
  i : De.bigstring;
  o : De.bigstring;
  tmp : Bytes.t;
  lines : string Queue.t;
  tail : Buffer.t;
}

let open_in path =
  let ic = Stdlib.open_in_bin path in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let tmp = Bytes.create De.io_buffer_size in
  let decoder = Gz.Inf.decoder `Manual ~o in
  { ic; decoder; i; o; tmp; lines = Queue.create (); tail = Buffer.create 2048 }

let close_in t = Stdlib.close_in t.ic
let close_in_noerr t = try Stdlib.close_in t.ic with _ -> ()

let with_open path f =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () -> f ic

let refill t =
  let len = Stdlib.input t.ic t.tmp 0 (Bytes.length t.tmp) in
  Bigstringaf.blit_from_bytes t.tmp ~src_off:0 t.i ~dst_off:0 ~len;
  t.decoder <- Gz.Inf.src t.decoder t.i 0 len

let contains_newline_in_bigstring o len =
  let rec loop i =
    if i >= len then false
    else if Bigstringaf.get o i = '\n' then true
    else loop (i + 1)
  in
  loop 0

let rec read_chunk t =
  match Gz.Inf.decode t.decoder with
  | `Await decoder ->
      t.decoder <- decoder;
      refill t;
      read_chunk t
  | `Flush decoder ->
      let len = De.bigstring_length t.o - Gz.Inf.dst_rem decoder in
      let data = Bigstringaf.substring t.o ~off:0 ~len in
      Buffer.add_string t.tail data;
      t.decoder <- Gz.Inf.flush decoder;
      `Continue (contains_newline_in_bigstring t.o len)
  | `End decoder ->
      let len = De.bigstring_length t.o - Gz.Inf.dst_rem decoder in
      if len > 0 then
        Buffer.add_string t.tail (Bigstringaf.substring t.o ~off:0 ~len);
      `End
  | `Malformed err -> failwith ("gzip error: " ^ err)

let find_nextline b s =
  let len = Buffer.length b in
  let rec loop i =
    if i >= len then raise Not_found
    else if Buffer.nth b i = '\n' then i
    else loop (i + 1)
  in
  loop s

let flush_tail ic =
  let len = Buffer.length ic.tail in
  let rec loop s =
    match find_nextline ic.tail s with
    | exception Not_found ->
        let b = Bytes.create (len - s) in
        Buffer.blit ic.tail s b 0 (len - s);
        Buffer.clear ic.tail;
        b
    | e ->
        let line_end =
          if e > s && Buffer.nth ic.tail (e - 1) = '\r' then e - 1 else e
        in
        Queue.push (Buffer.sub ic.tail s (line_end - s)) ic.lines;
        loop (e + 1)
  in
  loop 0

let rec read_until_newline ic =
  match read_chunk ic with
  | `End -> false
  | `Continue has_nl -> if has_nl then true else read_until_newline ic

let rec input_line ic =
  if not @@ Queue.is_empty ic.lines then Queue.pop ic.lines
  else
    let has_more = read_until_newline ic in
    let b = flush_tail ic in
    if (not has_more) && Bytes.length b = 0 then raise End_of_file
    else if not has_more then Bytes.to_string b
    else (
      Buffer.add_bytes ic.tail b;
      input_line ic)

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

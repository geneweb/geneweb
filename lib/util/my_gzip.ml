type in_channel = {
  gic : Gzip.in_channel;
  (* Underlying input channel of camlzip. *)
  lines : string Queue.t;
  (* Queue of complete lines not yet returned by [input_line]. *)
  tail : Buffer.t; (* Last incomplete line. *)
}

let open_in path =
  {
    gic = Gzip.open_in path;
    lines = Queue.create ();
    tail = Buffer.create 2_048;
  }

let close_in ic = Gzip.close_in ic.gic
let close_in_noerr ic = try Gzip.close_in ic.gic with _ -> ()

let with_open path f =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () -> f ic

let contains_newline b r =
  let rec loop i =
    if i >= r then false
    else if Char.equal (Bytes.get b i) '\n' then true
    else loop (i + 1)
  in
  loop 0

(* Search the end of the next line in the buffer [b] starting at [s]. *)
let find_nextline b s =
  let len = Buffer.length b in
  let rec loop i =
    if i >= len then raise Not_found
    else if Char.equal (Buffer.nth b i) '\n' then i
    else loop (i + 1)
  in
  loop s

(* Read from the input channel [ic] until a new line is found in the
   buffer. Return the number of read characters. *)
let read_until_newline ic =
  let sz = 8192 in
  let buf = Bytes.create sz in
  let rec loop acc =
    let r = Gzip.input ic.gic buf 0 sz in
    Buffer.add_subbytes ic.tail buf 0 r;
    if contains_newline buf r || r = 0 then acc + r else loop (acc + r)
  in
  loop 0

(* Feed the queue with all the complete lines in the buffer.
   The eventual last incomplete line is returned. The buffer tail
   is empty after calling this function.

   Return the incomplete line remaining in the buffer. *)
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
        Queue.push (Buffer.sub ic.tail s (e - s)) ic.lines;
        loop (e + 1)
  in
  loop 0

let rec input_line ic =
  if not @@ Queue.is_empty ic.lines then Queue.pop ic.lines
  else
    let r = read_until_newline ic in
    let b = flush_tail ic in
    if r = 0 && Bytes.length b = 0 then raise End_of_file
    else if r = 0 then Bytes.to_string b
    else (
      Buffer.add_bytes ic.tail b;
      input_line ic)

let gzip_string ?(level = 6) input =
  let input_len = String.length input in
  if input_len = 0 then ""
  else
    let header = Bytes.create 10 in
    Bytes.set header 0 '\x1f';
    Bytes.set header 1 '\x8b';
    Bytes.set header 2 '\x08';
    Bytes.set header 3 '\x00';
    Bytes.set header 4 '\x00';
    Bytes.set header 5 '\x00';
    Bytes.set header 6 '\x00';
    Bytes.set header 7 '\x00';
    Bytes.set header 8 '\x00';
    Bytes.set header 9 '\xff';
    let output_buf = Buffer.create (input_len / 2) in
    let input_pos = ref 0 in
    let refill buf =
      let available = input_len - !input_pos in
      let to_read = min available (Bytes.length buf) in
      Bytes.blit_string input !input_pos buf 0 to_read;
      input_pos := !input_pos + to_read;
      to_read
    in
    let flush buf len = Buffer.add_subbytes output_buf buf 0 len in
    Zlib.compress ~level ~header:false refill flush;
    let crc = Zlib.update_crc_string 0l input 0 input_len in
    let trailer = Bytes.create 8 in
    Bytes.set trailer 0 (Char.chr (Int32.to_int crc land 0xff));
    Bytes.set trailer 1
      (Char.chr (Int32.to_int (Int32.shift_right_logical crc 8) land 0xff));
    Bytes.set trailer 2
      (Char.chr (Int32.to_int (Int32.shift_right_logical crc 16) land 0xff));
    Bytes.set trailer 3
      (Char.chr (Int32.to_int (Int32.shift_right_logical crc 24) land 0xff));
    Bytes.set trailer 4 (Char.chr (input_len land 0xff));
    Bytes.set trailer 5 (Char.chr ((input_len lsr 8) land 0xff));
    Bytes.set trailer 6 (Char.chr ((input_len lsr 16) land 0xff));
    Bytes.set trailer 7 (Char.chr ((input_len lsr 24) land 0xff));
    Bytes.to_string header ^ Buffer.contents output_buf
    ^ Bytes.to_string trailer

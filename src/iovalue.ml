(* camlp5r ./q_codes.cmo *)
(* $Id: iovalue.ml,v 5.15 2012-01-27 16:27:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* Input:
   read inside a value output by output_value (no headers) must
   match OCaml's input_value system (intern.c) *)

value sizeof_long = 4;
value sign_extend_shift = (Sys.word_size / 8 - 1) * 8 - 1;
value sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift;

type in_funs 'a =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> bytes -> int -> int -> unit }
;

value input_binary_int64 ifuns ic =
  loop 8 0 where rec loop cnt n =
    if cnt = 0 then n
    else loop (cnt - 1) (n lsl 8 + ifuns.input_byte ic)
;

value rec input_loop ifuns ic =
  let code = ifuns.input_byte ic in
  if code >= <<PREFIX_SMALL_INT>> then
    if code >= <<PREFIX_SMALL_BLOCK>> then
      input_block ifuns ic (code land 0xf) ((code lsr 4) land 0x7)
    else Obj.magic (code land 0x3f)
  else if code >= <<PREFIX_SMALL_STRING>> then
    let len = code land 0x1F in
    let s = Bytes.create len in
    do { ifuns.input ic s 0 len; Obj.magic s }
  else
    match code with
    [ <<CODE_INT8>> -> Obj.magic (sign_extend (ifuns.input_byte ic))
    | <<CODE_INT16>> ->
        let h = ifuns.input_byte ic in
        Obj.magic ((sign_extend h) lsl 8 + ifuns.input_byte ic)
    | <<CODE_INT32>> ->
        let x1 = ifuns.input_byte ic in
        let x2 = ifuns.input_byte ic in
        let x3 = ifuns.input_byte ic in
        let x4 = ifuns.input_byte ic in
        Obj.magic ((sign_extend x1) lsl 24 + x2 lsl 16 + x3 lsl 8 + x4)
    | <<CODE_BLOCK32>> ->
        let header = ifuns.input_binary_int ic in
        Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
    | <<CODE_BLOCK64>> ->
        if Sys.word_size = 64 then
          let header = input_binary_int64 ifuns ic in
          Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
        else failwith "input bad code block 64"
    | <<CODE_STRING8>> ->
        let len = ifuns.input_byte ic in
        let s = Bytes.create len in
        do { ifuns.input ic s 0 len; Obj.magic s }
    | <<CODE_STRING32>> ->
        let len = ifuns.input_binary_int ic in
        let s = Bytes.create len in
        do { ifuns.input ic s 0 len; Obj.magic s }
    | code -> failwith (Printf.sprintf "input bad code 0x%x" code) ]
and input_block ifuns ic tag size =
  let v =
    if tag = 0 then Obj.magic (Array.make size (Obj.magic 0))
    else Obj.new_block tag size
  in
  do {
    for i = 0 to size - 1 do {
      let x = input_loop ifuns ic in
      Obj.set_field v i (Obj.magic x);
    };
    v
  }
;

value in_channel_funs =
  {input_byte = input_byte;
   input_binary_int = input_binary_int;
   input = really_input}
;

value input ic = Obj.magic (input_loop in_channel_funs ic);
value gen_input ifuns i = Obj.magic (input_loop ifuns i);

(* Output *)

type out_funs 'a =
  { output_byte : 'a -> int -> unit;
    output_binary_int : 'a -> int -> unit;
    output : 'a -> string -> int -> int -> unit }
;

value size_32 = ref 0;
value size_64 = ref 0;

value gen_output_block_header ofuns oc tag size =
  do {
    let hd = size lsl 10 + tag in
    if tag < 16 && size < 8 then
      ofuns.output_byte oc (<<PREFIX_SMALL_BLOCK>> + tag + size lsl 4)
    else if Sys.word_size = 64 && hd >= 1 lsl 32 then do {
      ofuns.output_byte oc <<CODE_BLOCK64>>;
      for i = 1 to 8 do {
        ofuns.output_byte oc (hd lsr (64 - 8 * i) land 0xFF);
      };
    }
    else do {
      ofuns.output_byte oc <<CODE_BLOCK32>>;
      (* hd = size << 10 + tag *)
      ofuns.output_byte oc (size lsr 14 land 0xFF);
      ofuns.output_byte oc (size lsr 6 land 0xFF);
      ofuns.output_byte oc (size lsl 2 land 0xFF);
      ofuns.output_byte oc (size lsl 10 land 0xFF + tag);
    };
    if size = 0 then ()
    else do {
      size_32.val := size_32.val + 1 + size;
      size_64.val := size_64.val + 1 + size;
    }
  }
;

value rec output_loop ofuns oc x =
  if Obj.is_int x then
    if Obj.magic x >= 0 && Obj.magic x < 0x40 then
      ofuns.output_byte oc (<<PREFIX_SMALL_INT>> + Obj.magic x)
    else if Obj.magic x >= -128 && Obj.magic x < 128 then do {
      ofuns.output_byte oc <<CODE_INT8>>;
      ofuns.output_byte oc (Obj.magic x);
    }
    else if Obj.magic x >= -32768 && Obj.magic x < 32768 then do {
      ofuns.output_byte oc <<CODE_INT16>>;
      ofuns.output_byte oc (Obj.magic x lsr 8);
      ofuns.output_byte oc (Obj.magic x);
    }
    else do {
      ofuns.output_byte oc <<CODE_INT32>>;
      ofuns.output_binary_int oc (Obj.magic x);
    }
  else
    if Obj.tag x = Obj.string_tag then do {
      let len = String.length (Obj.magic x) in
      if len < 0x20 then
        ofuns.output_byte oc (<<PREFIX_SMALL_STRING>> + len)
      else if len < 0x100 then do {
        ofuns.output_byte oc <<CODE_STRING8>>;
        ofuns.output_byte oc len;
      }
      else do {
        ofuns.output_byte oc <<CODE_STRING32>>;
        ofuns.output_binary_int oc len;
      };
      ofuns.output oc (Obj.magic x) 0 len;
      size_32.val := size_32.val + 1 + (len + 4) / 4;
      size_64.val := size_64.val + 1 + (len + 8) / 8;
    }
    else if Obj.tag x = Obj.double_tag || Obj.tag x = Obj.double_array_tag then
      failwith "Iovalue.output: floats not implemented"
    else if Obj.tag x = Obj.closure_tag then
      failwith "Iovalue.output <fun>"
    else if Obj.tag x = Obj.abstract_tag then
      failwith "Iovalue.output <abstract>"
    else if Obj.tag x = Obj.infix_tag then
      failwith "Iovalue.output: <infix>"
    else if Obj.tag x = Obj.custom_tag then
      failwith "Iovalue.output: <custom>"
    else if Obj.tag x = Obj.out_of_heap_tag then
      failwith "Iovalue.output: abstract value (outside heap)"
    else do {
      gen_output_block_header ofuns oc (Obj.tag x) (Obj.size x);
      (* last case of "for" separated, to make more tail recursive cases
         when last field is itself, to prevent some stacks overflows *)
      if Obj.size x > 0 then do {
        for i = 0 to Obj.size x - 2 do {
          output_loop ofuns oc (Obj.field x i);
        };
        output_loop ofuns oc (Obj.field x (Obj.size x - 1))
      }
      else ();
    }
;

value out_channel_funs =
  {output_byte = output_byte;
   output_binary_int = output_binary_int;
   output = output_substring}
;

value output oc x = output_loop out_channel_funs oc (Obj.repr x);
value gen_output ofuns i x = output_loop ofuns i (Obj.repr x);
value output_block_header = gen_output_block_header out_channel_funs;

(* Size *)

value size_funs =
  {output_byte = fun r _ -> incr r;
   output_binary_int = fun r _ -> r.val := r.val + 4;
   output = fun r _ beg len -> r.val := r.val + len - beg}
;

value size = ref 0;

value size v =
  do { size.val := 0; gen_output size_funs size v; size.val }
;

(* Digest *)

value dbuf = ref (Bytes.create 256);
value dlen = ref 0;
value dput_char c =
  do {
    if dlen.val = Bytes.length dbuf.val then do {
      dbuf.val := Bytes.extend dbuf.val 0 dlen.val;
    }
    else ();
    Bytes.set dbuf.val dlen.val c;
    incr dlen;
  }
;
value rec dput_int i =
  if i = 0 then ()
  else do {
    dput_char (Char.chr (Char.code '0' + i mod 10));
    dput_int (i / 10);
  }
;
value dput_string s =
  for i = 0 to String.length s - 1 do {
    dput_char s.[i];
  }
;

value rec digest_loop v =
  if not (Obj.is_block v) then
    let n = (Obj.magic v : int) in
    do { dput_char 'I'; dput_int n }
  else if Obj.tag v = Obj.closure_tag then
    invalid_arg "Iovalue.digest: closure"
  else if Obj.size v = 0 then
    do { dput_char 'T'; dput_int (Obj.tag v) }
  else if Obj.tag v = Obj.string_tag then do {
    let s = (Obj.magic v : string) in
    dput_char 'S'; dput_int (String.length s);
    dput_char '/'; dput_string s;
  }
  else do {
    dput_char 'O'; dput_int (Obj.tag v);
    dput_char '/'; dput_int (Obj.size v);
    digest_fields v 0;
  }
and digest_fields v i =
  if i = Obj.size v then ()
  else do { digest_loop (Obj.field v i); digest_fields v (i + 1) }
;

value digest v =
  do {
    dlen.val := 0;
    digest_loop (Obj.repr v);
    Digest.to_hex (Digest.subbytes dbuf.val 0 dlen.val)
  }
;

value output_value_header_size = 20;
value array_header_size arr_len =
  if arr_len < 8 then 1
  else if Sys.word_size = 64 && arr_len lsl 10 >= 1 lsl 32 then 9
  else 5
;

value output_array_access oc arr_get arr_len pos =
  loop (pos + output_value_header_size + array_header_size arr_len) 0
  where rec loop pos i =
    if i = arr_len then pos
    else do {
      output_binary_int oc pos;
      loop (pos + size (arr_get i)) (i + 1)
    }
;

(* *)

type header_pos = (int * int);

value intext_magic_number = [| 0x84; 0x95; 0xA6; 0xBE |];

value create_output_value_header oc = do {
  (* magic number *)
  for i = 0 to 3 do { output_byte oc intext_magic_number.(i); };
  let pos_header = pos_out oc in
  (* room for block length *)
  output_binary_int oc 0;
  (* room for obj counter *)
  output_binary_int oc 0;
  (* room for size_32 *)
  output_binary_int oc 0;
  (* room for size_64 *)
  output_binary_int oc 0;
  size_32.val := 0;
  size_64.val := 0;
  (pos_header, pos_out oc)
};

value patch_output_value_header oc (pos_header, pos_start) = do {
  let pos_end = pos_out oc in
  if Sys.word_size = 64 &&
     (pos_end >= 1 lsl 32 ||
      size_32.val >= 1 lsl 32 ||
      size_64.val >= 1 lsl 32)
  then failwith "Iovalue.output: object too big"
  else ();
  (* block_length *)
  seek_out oc pos_header;
  output_binary_int oc (pos_end - pos_start);
  (* obj counter is zero because no_sharing *)
  output_binary_int oc 0;
  (* size_32 *)
  output_binary_int oc size_32.val;
  (* size_64 *)
  output_binary_int oc size_64.val;
  pos_end;
};

(* camlp4r ./q_codes.cmo *)
(* $Id: iovalue.ml,v 3.3 2001-01-06 09:55:57 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

value string_tag = Obj.tag (Obj.repr "a");
value float_tag = Obj.tag (Obj.repr 3.5);
value fun_tag = Obj.tag (Obj.repr (fun x -> x));

(* Input:
   read inside a value output by output_value (no headers) must
   match OCaml's input_value system (intern.c) *)

value sizeof_long = 4;
value sign_extend_shift = (sizeof_long - 1) * 8 - 1;
value sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift;

type in_funs 'a =
  { input_byte : 'a -> int;
    input_binary_int : 'a -> int;
    input : 'a -> string -> int -> int -> unit }
;

value rec input_loop ifuns ic =
  let code = ifuns.input_byte ic in
  if code >= <<PREFIX_SMALL_INT>> then
    if code >= <<PREFIX_SMALL_BLOCK>> then
      input_block ifuns ic (code land 0xf) ((code lsr 4) land 0x7)
    else Obj.magic (code land 0x3f)
  else if code >= <<PREFIX_SMALL_STRING>> then
    let len = code land 0x1F in
    let s = String.create len in
    do ifuns.input ic s 0 len; return Obj.magic s
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
    | <<CODE_STRING8>> ->
        let len = ifuns.input_byte ic in
        let s = String.create len in
        do ifuns.input ic s 0 len; return Obj.magic s
    | <<CODE_STRING32>> ->
        let len = ifuns.input_binary_int ic in
        let s = String.create len in
        do ifuns.input ic s 0 len; return Obj.magic s
    | code -> failwith (Printf.sprintf "input bad code 0x%x" code) ]
and input_block ifuns ic tag size =
  let v =
    if tag == 0 then Obj.magic (Array.create size (Obj.magic 0))
    else Obj.new_block tag size
  in
  do for i = 0 to size - 1 do
       let x = input_loop ifuns ic in
       Obj.set_field v i (Obj.magic x);
     done;
  return v
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

value rec output_loop ofuns oc x =
  if not (Obj.is_block x) then
    if Obj.magic x >= 0 && Obj.magic x < 0x40 then
      ofuns.output_byte oc (<<PREFIX_SMALL_INT>> + Obj.magic x)
    else if Obj.magic x >= -128 && Obj.magic x < 128 then
      do ofuns.output_byte oc <<CODE_INT8>>;
         ofuns.output_byte oc (Obj.magic x);
      return ()
    else if Obj.magic x >= -32768 && Obj.magic x < 32768 then
      do ofuns.output_byte oc <<CODE_INT16>>;
         ofuns.output_byte oc (Obj.magic x lsr 8);
         ofuns.output_byte oc (Obj.magic x);
      return ()
    else
      do ofuns.output_byte oc <<CODE_INT32>>; return
      ofuns.output_binary_int oc (Obj.magic x)
  else
    if Obj.tag x == fun_tag then failwith "Iovalue.output <fun>"
    else if Obj.tag x == string_tag then
      let len = String.length (Obj.magic x) in
      do if len < 0x20 then
           ofuns.output_byte oc (<<PREFIX_SMALL_STRING>> + len)
         else if len < 0x100 then
           do ofuns.output_byte oc <<CODE_STRING8>>;
              ofuns.output_byte oc len;
           return ()
         else
           do ofuns.output_byte oc <<CODE_STRING32>>;
              ofuns.output_binary_int oc len;
           return ();
         ofuns.output oc (Obj.magic x) 0 len;
      return ()
    else if Obj.tag x == float_tag then
      failwith "Iovalue.output: floats not implemented"
    else
      do if Obj.tag x < 16 && Obj.size x < 8 then
           ofuns.output_byte oc
             (<<PREFIX_SMALL_BLOCK>> + Obj.tag x + Obj.size x lsl 4)
         else
           do ofuns.output_byte oc <<CODE_BLOCK32>>;
              ofuns.output_binary_int oc (Obj.tag x + Obj.size x lsl 10);
           return ();
         for i = 0 to Obj.size x - 1 do
           output_loop ofuns oc (Obj.field x i);
         done;
      return ()
;

value out_channel_funs =
  {output_byte = output_byte;
   output_binary_int = output_binary_int;
   output = output}
;

value output oc x = output_loop out_channel_funs oc (Obj.repr x);
value gen_output ofuns i x = output_loop ofuns i (Obj.repr x);

(* Size *)

value size_funs =
  {output_byte = fun r _ -> incr r;
   output_binary_int = fun r _ -> r.val := r.val + 4;
   output = fun r _ beg len -> r.val := r.val + len - beg}
;

value size = ref 0;

value size v =
  do size.val := 0;
     gen_output size_funs size v;
  return size.val;

(* Digest *)

value dbuf = ref (String.create 256);
value dlen = ref 0;
value dput_char c =
  do if dlen.val = String.length dbuf.val then
       let nlen = 2 * dlen.val in
       let ndbuf = String.create nlen in
       do String.blit dbuf.val 0 ndbuf 0 dlen.val; dbuf.val := ndbuf; return ()
     else ();
     dbuf.val.[dlen.val] := c;
     incr dlen;
  return ()
;
value rec dput_int i =
  if i == 0 then ()
  else
    do dput_char (Char.chr (Char.code '0' + i mod 10)); return
    dput_int (i / 10)
;
value dput_string s =
  for i = 0 to String.length s - 1 do
    dput_char s.[i];
  done
;

value hexchar i =
  if i <= 9 then Char.chr (Char.code '0' + i)
  else Char.chr (Char.code 'A' + i - 10)
;

value string_code s =
  let r = String.create (String.length s * 2) in
  do for i = 0 to String.length s - 1 do
       r.[2*i] := hexchar (Char.code s.[i] / 16);
       r.[2*i+1] := hexchar (Char.code s.[i] mod 16);
     done;
  return r
;

value rec digest_loop v =
  if not (Obj.is_block v) then
    let n = (Obj.magic v : int) in
    do dput_char 'I'; dput_int n; return ()
  else if Obj.size v == 0 then
    do dput_char 'T'; dput_int (Obj.tag v); return ()
  else if Obj.tag v == string_tag then
    let s = (Obj.magic v : string) in
    do dput_char 'S'; dput_int (String.length s);
       dput_char '/'; dput_string s;
    return ()
  else
    do dput_char 'O'; dput_int (Obj.tag v);
       dput_char '/'; dput_int (Obj.size v);
       digest_fields v 0;
    return ()
and digest_fields v i =
  if i == Obj.size v then ()
  else do digest_loop (Obj.field v i); return digest_fields v (i + 1)
;

value digest v =
  do dlen.val := 0;
     digest_loop (Obj.repr v);
  return string_code (Digest.substring dbuf.val 0 dlen.val)
;

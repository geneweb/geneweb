(* $Id: iovalue.ml,v 5.15 2012-01-27 16:27:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* Input:
   read inside a value output by output_value (no headers) must
   match OCaml's input_value system (intern.c) *)

let sizeof_long = 4
let sign_extend_shift = (((Sys.word_size / 8) - 1) * 8) - 1
let sign_extend x = (x lsl sign_extend_shift) asr sign_extend_shift
let prefix_SMALL_BLOCK = 0x80
let prefix_SMALL_INT = 0x40
let prefix_SMALL_STRING = 0x20
let code_INT8 = 0x0
let code_INT16 = 0x1
let code_INT32 = 0x2
let code_INT64 = 0x3
let code_BLOCK32 = 0x8
let code_BLOCK64 = 0x13
let code_STRING8 = 0x9
let code_STRING32 = 0xA

type 'a in_funs = {
  input_byte : 'a -> int;
  input_binary_int : 'a -> int;
  input : 'a -> bytes -> int -> int -> unit;
}

let input_binary_int64 ifuns ic =
  let rec loop cnt n =
    if cnt = 0 then n else loop (cnt - 1) ((n lsl 8) + ifuns.input_byte ic)
  in
  loop 8 0

let rec input_loop ifuns ic =
  let code = ifuns.input_byte ic in
  if code >= prefix_SMALL_INT then
    if code >= prefix_SMALL_BLOCK then
      input_block ifuns ic (code land 0xf) ((code lsr 4) land 0x7)
    else Obj.magic (code land 0x3f)
  else if code >= prefix_SMALL_STRING then (
    let len = code land 0x1F in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else if code = code_INT8 then Obj.magic (sign_extend (ifuns.input_byte ic))
  else if code = code_INT16 then
    let h = ifuns.input_byte ic in
    Obj.magic ((sign_extend h lsl 8) + ifuns.input_byte ic)
  else if code = code_INT32 then
    let x1 = ifuns.input_byte ic in
    let x2 = ifuns.input_byte ic in
    let x3 = ifuns.input_byte ic in
    let x4 = ifuns.input_byte ic in
    Obj.magic ((sign_extend x1 lsl 24) + (x2 lsl 16) + (x3 lsl 8) + x4)
  else if code = code_INT64 then
    let () = assert (Sys.word_size = 64) in
    Obj.magic (input_binary_int64 ifuns ic)
  else if code = code_BLOCK32 then
    let header = ifuns.input_binary_int ic in
    Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
  else if code = code_BLOCK64 then
    if Sys.word_size = 64 then
      let header = input_binary_int64 ifuns ic in
      Obj.magic (input_block ifuns ic (header land 0xff) (header lsr 10))
    else failwith "input bad code block 64"
  else if code = code_STRING8 then (
    let len = ifuns.input_byte ic in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else if code = code_STRING32 then (
    let len = ifuns.input_binary_int ic in
    let s = Bytes.create len in
    ifuns.input ic s 0 len;
    Obj.magic s)
  else failwith (Printf.sprintf "input bad code 0x%x" code)

and input_block ifuns ic tag size =
  let v =
    if tag = 0 then Obj.magic (Array.make size (Obj.magic 0))
    else Obj.new_block tag size
  in
  for i = 0 to size - 1 do
    let x = input_loop ifuns ic in
    Obj.set_field v i (Obj.magic x)
  done;
  v

let in_channel_funs = { input_byte; input_binary_int; input = really_input }
let input ic = Obj.magic (input_loop in_channel_funs ic)

(* Output *)

type 'a out_funs = {
  output_byte : 'a -> int -> unit;
  output_binary_int : 'a -> int -> unit;
  output : 'a -> string -> int -> int -> unit;
}

let size_32 = ref 0
let size_64 = ref 0

let output_binary_int64 ofuns oc x =
  for i = 1 to 8 do
    ofuns.output_byte oc ((x lsr (64 - (8 * i))) land 0xFF)
  done

let gen_output_block_header ofuns oc tag size =
  let hd = (size lsl 10) + tag in
  if tag < 16 && size < 8 then
    ofuns.output_byte oc (prefix_SMALL_BLOCK + tag + (size lsl 4))
  else if Sys.word_size = 64 && hd >= 1 lsl 32 then (
    ofuns.output_byte oc code_BLOCK64;
    output_binary_int64 ofuns oc hd)
  else (
    ofuns.output_byte oc code_BLOCK32;
    (* hd = size << 10 + tag *)
    ofuns.output_byte oc ((size lsr 14) land 0xFF);
    ofuns.output_byte oc ((size lsr 6) land 0xFF);
    ofuns.output_byte oc ((size lsl 2) land 0xFF);
    ofuns.output_byte oc (((size lsl 10) land 0xFF) + tag));
  if size = 0 then ()
  else (
    size_32 := !size_32 + 1 + size;
    size_64 := !size_64 + 1 + size)

let rec output_loop ofuns oc x =
  if Obj.is_int x then
    if Obj.magic x >= 0 && Obj.magic x < 0x40 then
      ofuns.output_byte oc (prefix_SMALL_INT + Obj.magic x)
    else if Obj.magic x >= -128 && Obj.magic x < 128 then (
      ofuns.output_byte oc code_INT8;
      ofuns.output_byte oc (Obj.magic x))
    else if Obj.magic x >= -32768 && Obj.magic x <= 32767 then (
      ofuns.output_byte oc code_INT16;
      ofuns.output_byte oc (Obj.magic x lsr 8);
      ofuns.output_byte oc (Obj.magic x))
    else if Obj.magic x >= -1073741824 && Obj.magic x <= 1073741823 then (
      ofuns.output_byte oc code_INT32;
      ofuns.output_binary_int oc (Obj.magic x))
    else (
      ofuns.output_byte oc code_INT64;
      output_binary_int64 ofuns oc (Obj.magic x))
  else if Obj.tag x = Obj.string_tag then (
    let len = String.length (Obj.magic x) in
    if len < 0x20 then ofuns.output_byte oc (prefix_SMALL_STRING + len)
    else if len < 0x100 then (
      ofuns.output_byte oc code_STRING8;
      ofuns.output_byte oc len)
    else (
      ofuns.output_byte oc code_STRING32;
      ofuns.output_binary_int oc len);
    ofuns.output oc (Obj.magic x) 0 len;
    size_32 := !size_32 + 1 + ((len + 4) / 4);
    size_64 := !size_64 + 1 + ((len + 8) / 8))
  else if Obj.tag x = Obj.double_tag || Obj.tag x = Obj.double_array_tag then
    failwith "Iovalue.output: floats not implemented"
  else if Obj.tag x = Obj.closure_tag then failwith "Iovalue.output <fun>"
  else if Obj.tag x = Obj.abstract_tag then failwith "Iovalue.output <abstract>"
  else if Obj.tag x = Obj.infix_tag then failwith "Iovalue.output: <infix>"
  else if Obj.tag x = Obj.custom_tag then failwith "Iovalue.output: <custom>"
  else (
    gen_output_block_header ofuns oc (Obj.tag x) (Obj.size x);
    (* last case of "for" separated, to make more tail recursive cases
       when last field is itself, to prevent some stacks overflows *)
    if Obj.size x > 0 then (
      for i = 0 to Obj.size x - 2 do
        output_loop ofuns oc (Obj.field x i)
      done;
      output_loop ofuns oc (Obj.field x (Obj.size x - 1))))

let out_channel_funs =
  { output_byte; output_binary_int; output = output_substring }

let output oc x = output_loop out_channel_funs oc (Obj.repr x)
let gen_output ofuns i x = output_loop ofuns i (Obj.repr x)

(* Size *)

let size_funs =
  {
    output_byte = (fun r _ -> incr r);
    output_binary_int = (fun r _ -> r := !r + 4);
    output = (fun r _ beg len -> r := !r + len - beg);
  }

let size = ref 0

let size v =
  size := 0;
  gen_output size_funs size v;
  !size

let output_value_header_size = 20

let array_header_size arr_len =
  if arr_len < 8 then 1
  else if Sys.word_size = 64 && arr_len lsl 10 >= 1 lsl 32 then 9
  else 5

let output_array_access oc arr_get arr_len pos =
  let rec loop pos i =
    if i = arr_len then pos
    else (
      output_binary_int oc pos;
      loop (pos + size (arr_get i)) (i + 1))
  in
  loop (pos + output_value_header_size + array_header_size arr_len) 0

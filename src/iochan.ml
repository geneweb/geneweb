(* $Id: iochan.ml,v 5.5 2012-01-27 08:53:53 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type t = {iofd : Unix.file_descr; iopos : mutable int};

value openfile fname trunc =
  let fd =
    let trunc = if trunc then [Unix.O_TRUNC] else [] in
    Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT :: trunc] 0o644
  in
  {iofd = fd; iopos = 0}
;

value ib = String.make 4 ' ';

value input_byte ioc =
  let ret = Unix.read ioc.iofd ib 0 1 in
  if ret <> 1 then failwith "Iochan.input_byte"
  else do {
    ioc.iopos := ioc.iopos + 1;
    Char.code ib.[0]
  }
;

value input_binary_int ioc = do {
  let ret = Unix.read ioc.iofd ib 0 4 in
  if ret <> 4 then failwith "Iochan.input_binary_int" else ();
  ioc.iopos := ioc.iopos + 4;
  let r =
    Iovalue.sign_extend (Char.code ib.[0]) lsl 24 +
  Char.code ib.[1] lsl 16 +
  Char.code ib.[2] lsl 8 +
  Char.code ib.[3]
  in
  assert (r >= -0x40000000);
  assert (r <= 0x3fffffff);
  r
};

value input ioc buff start len =
  let ret = Unix.read ioc.iofd buff start len in
  if ret <> len then failwith "Iochan.input"
  else ioc.iopos := ioc.iopos + len
;

value iochan_in_funs =
  {Iovalue.input_byte = input_byte;
   input_binary_int = input_binary_int;
   input = input}
;

value input_value_no_header = Iovalue.gen_input iochan_in_funs;

value output_byte ioc w = do {
  Bytes.set ib 0 (Char.chr (w land 0xFF));
  let len = Unix.write ioc.iofd ib 0 1 in
  if len <> 1 then failwith "iochan_output_byte" else ();
  ioc.iopos := ioc.iopos + 1;
};

value output_binary_int ioc w = do {
  Bytes.set ib 0 (Char.chr (w lsr 24 land 0xFF));
  Bytes.set ib 1 (Char.chr (w lsr 16 land 0xFF));
  Bytes.set ib 2 (Char.chr (w lsr 8 land 0xFF));
  Bytes.set ib 3 (Char.chr (w land 0xFF));
  let len = Unix.write ioc.iofd ib 0 4 in
  if len <> 4 then failwith "iochan_output_binary_int" else ();
  ioc.iopos := ioc.iopos + 4;
};

value output ioc buff start len =
  let ret = Unix.write ioc.iofd buff start len in
  if ret <> len then failwith "Iochan.output"
  else ioc.iopos := ioc.iopos + len
;

value iochan_out_funs =
  {Iovalue.output_byte = output_byte;
   output_binary_int = output_binary_int;
   output = output}
;

value output_value_no_header ioc v = Iovalue.gen_output iochan_out_funs ioc v;

value seek ioc pos =
  if ioc.iopos = pos then ()
  else do {
    let _ : int = Unix.lseek ioc.iofd pos Unix.SEEK_SET in
    ioc.iopos := pos;
  }
;

value seek_end ioc = do {
  let pos = Unix.lseek ioc.iofd 0 Unix.SEEK_END in
  ioc.iopos := pos;
  pos
};

value close ioc = Unix.close ioc.iofd;

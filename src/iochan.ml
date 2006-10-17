(* $Id: iochan.ml,v 5.1 2006-10-17 13:03:52 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

type t = {iofd : Unix.file_descr; iopos : mutable int};

value openfile fname trunc =
  let fd =
    let trunc = if trunc then [Unix.O_TRUNC] else [] in
    Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT :: trunc] 0o644
  in
  {iofd = fd; iopos = 0}
;

value ib = String.make 4 ' ';
value input_binary_int ioc = do {
  let ret = Unix.read ioc.iofd ib 0 4 in
  if ret <> 4 then raise End_of_file else ();
  ioc.iopos := ioc.iopos + 4;
  Char.code ib.[0] lsl 24 +
  Char.code ib.[1] lsl 16 +
  Char.code ib.[2] lsl 8 +
  Char.code ib.[3]
};

value output_binary_int ioc w = do {
  ib.[0] := Char.chr (w lsr 24 land 0xFF);
  ib.[1] := Char.chr (w lsr 16 land 0xFF);
  ib.[2] := Char.chr (w lsr 8 land 0xFF);
  ib.[3] := Char.chr (w land 0xFF);
  let len = Unix.write ioc.iofd ib 0 4 in
  if len <> 4 then failwith "iochan_output_binary_int" else ();
  ioc.iopos := ioc.iopos + 4;
};

value seek ioc pos =
  if ioc.iopos = pos then ()
  else do {
    let _ : int = Unix.lseek ioc.iofd pos Unix.SEEK_SET in
    ioc.iopos := pos;
  }
;

value close ioc = Unix.close ioc.iofd;

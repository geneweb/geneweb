(* camlp4r *)
(* $Id: history.ml,v 2.1 1999-09-23 22:19:12 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value ext_flags =
  [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
;

value record conf base (fn, sn, occ) action =
  let bname =
    if Filename.check_suffix conf.bname ".gwb" then conf.bname
    else conf.bname ^ ".gwb"
  in
  let fname =
    List.fold_right Filename.concat [Util.base_dir.val; bname] "history"
  in
  let oc = open_out_gen ext_flags 0o644 fname in
  let (hh, mm, ss) = conf.time in
  do Printf.fprintf oc "%04d-%02d-%02d %02d:%02d:%02d %s %s.%d %s\n"
       conf.today.year conf.today.month conf.today.day hh mm ss action fn occ
         sn;
     close_out oc;
  return ()
;

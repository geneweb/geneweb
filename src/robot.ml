(* camlp4r *)
(* $Id: robot.ml,v 1.3 1999-08-06 04:06:13 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Util;

module W = Map.Make (struct type t = string ; value compare = compare; end);

type excl =
  { excl : mutable list (string * ref int);
    who : mutable W.t (list float * float * int) }
;

value nl () =
  ifdef UNIX then Wserver.wprint "\r\n" else Wserver.wprint "\n"
;

value robot_error from cnt sec =
  let title _ = Wserver.wprint "Access refused" in
  do Wserver.wprint "HTTP/1.0 403 Forbidden"; nl ();
     Wserver.wprint "Content-type: text/html; charset=iso-8859-1"; nl ();
     nl ();
     Wserver.wprint "<head><title>";
     title True;
     Wserver.wprint "</title>\n<body>\n<h1>";
     title False;
     Wserver.wprint "</h1>\n";
     Wserver.wprint "
You made more than %d requests in less than %d seconds.
Considering that you are probably a robot, your access has been disconnected.
" cnt sec;
     Wserver.wprint "</body>\n";
  return raise Exit
;

value purge_who tm xcl sec =
  let sec = float sec in
  let to_remove =
    W.fold
      (fun k (v, _, _) l ->
         match v with
         [ [tm0 :: _] -> if tm -. tm0 > sec then [k :: l] else l
         | [] -> [k :: l] ])
      xcl.who []
  in
  List.iter (fun k -> xcl.who := W.remove k xcl.who) to_remove
;

value fprintf_date oc tm =
  Printf.fprintf oc "%4d/%02d/%02d %02d:%02d:%02d"
    (1900 + tm.Unix.tm_year) (succ tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
;

value check oc tm from max_call sec =
  let fname = Srcfile.adm_file "robot" in
  let xcl =
    match try Some (open_in_bin fname) with _ -> None with
    [ Some ic ->
        let v = try input_value ic with _ -> {excl = []; who = W.empty} in
        do close_in ic; return v
    | None -> {excl = []; who = W.empty} ]
  in
  let refused =
    match try Some (List.assoc from xcl.excl) with [ Not_found -> None ] with
    [ Some att ->
        do incr att;
           if att.val mod max_call == 0 then
             do fprintf_date oc (Unix.localtime tm);
                Printf.fprintf oc "\n";
                Printf.fprintf oc "  From: %s\n" from;
                Printf.fprintf oc "  %d refused attempts;" att.val;
                Printf.fprintf oc " to restore access, delete file \"%s\"\n"
                  fname;
             return ()
           else ();
        return True
    | None ->
        do purge_who tm xcl sec; return
        let (r, _, _) =
          try W.find from xcl.who with [ Not_found -> ([], tm, 0) ]
        in
        let (cnt, r, t, tm0, nb) =
          count r where rec count =
            fun
            [ [t :: tl] ->
                if tm -. t < float sec then
                  let (cnt, tl, t1, tm0, nb) = count tl in
                  (cnt + 1, [t :: tl], if t1 = 0.0 then t else t1,
                   if tl = [] then t else tm0, nb + 1)
                else (1, [], t, t, 0)
            | [] -> (1, [], 0.0, tm, 0) ]
        in
        do xcl.who := W.add from ([tm :: r], tm0, nb + 1) xcl.who; return
        let refused =
          if cnt > max_call then
            do Printf.fprintf oc " --- %s is a robot" from;
               Printf.fprintf oc
                 " (%d > %d connections in %g <= %d seconds)\n" cnt
                 max_call (tm -. t) sec; flush Pervasives.stderr;
               xcl.excl := [(from, ref 1) :: xcl.excl];
               xcl.who := W.remove from xcl.who;
            return True
          else False
        in
        do if xcl.excl <> [] then
             do List.iter
                  (fun (s, att) ->
                     do Printf.fprintf oc " --- definitively refused:";
                        Printf.fprintf oc " %s (%d refused attempts)\n"
                          s att.val;
                     return ())
                  xcl.excl;
                Printf.fprintf oc
                  " --- to restore access, delete file \"%s\"\n" fname;
             return ()
           else ();
           W.iter
             (fun k (_, tm0, nb) ->
                Printf.fprintf oc
                  " --- addr %s: %d requests since %.0f seconds\n" k nb
                  (tm -. tm0))
              xcl.who;
        return refused ]
  in
  do match try Some (open_out_bin fname) with [ Sys_error _ -> None ] with
     [ Some oc -> do output_value oc xcl; close_out oc; return ()
     | None -> () ];
  return
  if refused then robot_error from max_call sec else ()
;

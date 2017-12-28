(* camlp5r *)
(* $Id: robot.ml,v 5.17 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Util;
open Printf;

value magic_robot = "GWRB0007";

module W = Map.Make (struct type t = string; value compare = compare; end);

type norfriwiz = [ Normal | Friend of string | Wizard of string ];

type who =
  { acc_times : list float;
    oldest_time : float;
    nb_connect : int;
    nbase : string;
    utype : norfriwiz }
;

type excl =
  { excl : mutable list (string * ref int);
    who : mutable W.t who;
    max_conn : mutable (int * string) }
;

value robot_error conf from cnt sec =
  do {
    if not conf.cgi then Wserver.http "403 Forbidden" else ();
    Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
    Util.nl ();
    Util.nl ();
    let env =
      [("cnt", string_of_int cnt) ; ("sec", string_of_int sec)]
    in
    match open_etc_file "robot" with
    [ Some ic -> Templ.copy_from_templ conf env ic
    | None ->
        let title _ = Wserver.wprint "Access refused" in
        do {
          Wserver.wprint "<head><title>";
          title True;
          Wserver.wprint "</title>\n<body>\n<h1>";
          title False;
          Wserver.wprint "</body>\n";
        } ];
    raise Exit
  }
;

value purge_who tm xcl sec =
  let sec = float sec in
  let to_remove =
    W.fold
      (fun k who l ->
         match who.acc_times with
         [ [tm0 :: _] -> if tm -. tm0 > sec then [k :: l] else l
         | [] -> [k :: l] ])
      xcl.who []
  in
  List.iter (fun k -> xcl.who := W.remove k xcl.who) to_remove
;

value fprintf_date oc tm =
  fprintf oc "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;

value input_excl ic =
  let b = really_input_string ic (String.length magic_robot) in
  if b <> magic_robot then raise Not_found else (input_value ic : excl)
;

value output_excl oc xcl =
  do {
    output_string oc magic_robot;
    output_value oc (xcl : excl);
  }
;

value robot_excl () =
  let fname = Srcfile.adm_file "robot" in
  let xcl =
    match try Some (Secure.open_in_bin fname) with _ -> None with
    [ Some ic ->
        let v =
          try input_excl ic with _ ->
            {excl = []; who = W.empty; max_conn = (0, "")}
        in
        do { close_in ic; v }
    | None -> {excl = []; who = W.empty; max_conn = (0, "")} ]
  in
  (xcl, fname)
;

value min_disp_req = ref 6;

value check oc tm from max_call sec conf suicide =
  let nfw =
    if conf.wizard then Wizard conf.user
    else if conf.friend then Friend conf.user
    else Normal
  in
  let (xcl, fname) = robot_excl () in
  let refused =
    match try Some (List.assoc from xcl.excl) with [ Not_found -> None ] with
    [ Some att ->
        do {
          incr att;
          if att.val mod max_call = 0 then do {
            fprintf_date oc (Unix.localtime tm);
            fprintf oc "\n";
            fprintf oc "  From: %s\n" from;
            fprintf oc "  %d refused attempts;" att.val;
            fprintf oc " to restore access, delete file \"%s\"\n"
              fname;
          }
          else ();
          True
        }
    | None ->
        do {
          purge_who tm xcl sec;
          let r =
            try (W.find from xcl.who).acc_times with [ Not_found -> [] ]
          in
          let (cnt, tml, tm0) =
            let sec = float sec in
            let rec count cnt tml =
              fun
              [ [] -> (cnt, tml, tm)
              | [tm1] ->
                  if tm -. tm1 < sec then (cnt + 1, [tm1 :: tml], tm1)
                  else (cnt, tml, tm1)
              | [tm1 :: tml1] ->
                  if tm -. tm1 < sec then count (cnt + 1) [tm1 :: tml] tml1
                  else (cnt, tml, tm1) ]
            in
            count 1 [] r
          in
          let r = List.rev tml in
          xcl.who :=
            W.add from
              {acc_times = [tm :: r]; oldest_time = tm0; nb_connect = cnt;
               nbase = conf.bname; utype = nfw}
              xcl.who;
          let refused =
            if suicide || cnt > max_call then do {
              fprintf oc "--- %s is a robot" from;
              if suicide then
                fprintf oc " (called the \"suicide\" request)\n"
              else
                fprintf oc
                  " (%d > %d connections in %g <= %d seconds)\n" cnt max_call
                  (tm -. tm0) sec;
              flush Pervasives.stderr;
              xcl.excl := [(from, ref 1) :: xcl.excl];
              xcl.who := W.remove from xcl.who;
              xcl.max_conn := (0, "");
              True
            }
            else False
          in
          if xcl.excl <> [] then do {
            List.iter
              (fun (s, att) ->
                 do {
                   fprintf oc "--- excluded:";
                   fprintf oc " %s (%d refused attempts)\n" s att.val;
                   ()
                 })
              xcl.excl;
            fprintf oc "--- to restore access, delete file \"%s\"\n"
              fname;
          }
          else ();
          let (list, nconn) =
            W.fold
              (fun k w (list, nconn) ->
                 let tm = w.oldest_time in
                 let nb = w.nb_connect in
                 do {
                   if nb > fst xcl.max_conn then xcl.max_conn := (nb, k)
                   else ();
                   (if nb < min_disp_req.val then list
                    else [(k, tm, nb) :: list],
                    nconn + 1)
                 })
              xcl.who ([], 0)
          in
          let list =
            List.sort
              (fun (_, tm1, nb1) (_, tm2, nb2) ->
                 match compare nb2 nb1 with
                 [ 0 -> compare tm2 tm1
                 | x -> x ])
              list
          in
          List.iter
            (fun (k, tm0, nb) ->
               fprintf oc "--- %3d req - %3.0f sec - %s\n" nb
                 (tm -. tm0) k)
            list;
          fprintf oc "--- max %d req by %s / conn %d\n"
            (fst xcl.max_conn) (snd xcl.max_conn) nconn;
          refused
        } ]
  in
  do {
    match
      try Some (Secure.open_out_bin fname) with [ Sys_error _ -> None ]
    with
    [ Some oc -> do { output_excl oc xcl; close_out oc; }
    | None -> () ];
    if refused then robot_error conf from max_call sec else ();
    W.fold
      (fun _ w (c, cw, cf, wl) ->
         if w.nbase = conf.bname && w.nbase <> "" then
           match w.utype with
           [ Wizard n ->
               let at = List.hd w.acc_times in
               if List.mem_assoc n wl then
                 let old_at = List.assoc n wl in
                 if at > old_at then
                   let wl = List.remove_assoc n wl in
                   (c, cw, cf, [(n, at) :: wl])
                 else (c, cw, cf, wl)
               else (c + 1, cw + 1, cf, [(n, at) :: wl])
           | Friend _ ->
               if w.nb_connect > 2 then (c + 1, cw, cf + 1, wl)
               else (c, cw, cf, wl)
           | Normal ->
               if w.nb_connect > 2 then (c + 1, cw, cf, wl)
               else (c, cw, cf, wl) ]
         else (c, cw, cf, wl))
      xcl.who (0, 0, 0, [])
  }
;

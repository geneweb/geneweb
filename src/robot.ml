(* camlp4r *)
(* $Id: robot.ml,v 4.3 2001-04-21 15:51:29 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Util;

value magic_robot = "GWRB0002";

module W = Map.Make (struct type t = string; value compare = compare; end);

type excl =
  { excl : mutable list (string * ref int);
    who : mutable W.t (list float * float * int);
    max_conn : mutable (int * string) }
;

value robot_error cgi from cnt sec =
  do {
    if cgi then ()
    else do { Wserver.wprint "HTTP/1.0 403 Forbidden"; Util.nl (); };
    Wserver.wprint "Content-type: text/html; charset=iso-8859-1";
    Util.nl ();
    Util.nl ();
    let env =
      [('c', fun _ -> string_of_int cnt); ('s', fun _ -> string_of_int sec)]
    in
    match open_etc_file "robot" with
    [ Some ic -> copy_from_etc env "geneweb" ic
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
      (fun k (v, _, _) l ->
         match v with
         [ [tm0 :: _] -> if tm -. tm0 > sec then [k :: l] else l
         | [] -> [k :: l] ])
      xcl.who []
  in
  List.iter (fun k -> xcl.who := W.remove k xcl.who) to_remove
;

value fprintf_date oc tm =
  Printf.fprintf oc "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;

value input_excl =
  let b = String.create (String.length magic_robot) in
  fun ic ->
    do {
      really_input ic b 0 (String.length b);
      if b <> magic_robot then raise Not_found else (input_value ic : excl)
    }
;

value robot_excl () =
  let fname = Srcfile.adm_file "robot" in
  let xcl =
    match try Some (open_in_bin fname) with _ -> None with
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

value check oc tm from max_call sec cgi suicide =
  let (xcl, fname) = robot_excl () in
  let refused =
    match try Some (List.assoc from xcl.excl) with [ Not_found -> None ] with
    [ Some att ->
        do {
          incr att;
          if att.val mod max_call == 0 then do {
            fprintf_date oc (Unix.localtime tm);
            Printf.fprintf oc "\n";
            Printf.fprintf oc "  From: %s\n" from;
            Printf.fprintf oc "  %d refused attempts;" att.val;
            Printf.fprintf oc " to restore access, delete file \"%s\"\n"
              fname;
          }
          else ();
          True
        }
    | None ->
        do {
          purge_who tm xcl sec;
          let (r, _, _) =
            try W.find from xcl.who with [ Not_found -> ([], tm, 0) ]
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
          xcl.who := W.add from ([tm :: r], tm0, cnt) xcl.who;
          let refused =
            if suicide || cnt > max_call then do {
              Printf.fprintf oc "--- %s is a robot" from;
              if suicide then
                Printf.fprintf oc " (called the \"suicide\" request)\n"
              else
                Printf.fprintf oc
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
                   Printf.fprintf oc "--- excluded:";
                   Printf.fprintf oc " %s (%d refused attempts)\n" s att.val;
                   ()
                 })
              xcl.excl;
            Printf.fprintf oc "--- to restore access, delete file \"%s\"\n"
              fname;
          }
          else ();
          let (list, nconn) =
            W.fold
              (fun k (_, tm, nb) (list, nconn) ->
                 (if nb <= 5 then list else [(k, tm, nb) :: list],
                  nconn + 1))
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
               do {
                 Printf.fprintf oc "--- %3d req - %3.0f sec - %s\n" nb
                   (tm -. tm0) k;
                 if nb > fst xcl.max_conn then xcl.max_conn := (nb, k)
                 else ();
               })
            list;
          Printf.fprintf oc "--- conn %d\n" nconn;
          Printf.fprintf oc "--- max %d req by %s\n" (fst xcl.max_conn)
            (snd xcl.max_conn);
          refused
        } ]
  in
  do {
    match try Some (open_out_bin fname) with [ Sys_error _ -> None ] with
    [ Some oc ->
        do {
          output_string oc magic_robot; output_value oc xcl; close_out oc;
        }
    | None -> () ];
    if refused then robot_error cgi from max_call sec else ()
  }
;

(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config

let magic_robot = "GWRB0007"

module W = Map.Make (struct type t = string let compare = compare end)

type norfriwiz =
    Normal
  | Friend of string
  | Wizard of string

type who =
  { acc_times : float list;
    oldest_time : float;
    nb_connect : int;
    nbase : string;
    utype : norfriwiz }

type excl =
  { mutable excl : (string * int ref) list;
    mutable who : who W.t;
    mutable max_conn : int * string }

let robot_error conf cnt sec =
  Wserver.http Wserver.Forbidden;
  Wserver.header "Content-type: text/html; charset=iso-8859-1";
  let env = ["cnt", string_of_int cnt; "sec", string_of_int sec] in
  Util.include_template conf env "robot"
    (fun () ->
      let title _ = Wserver.printf "Access refused" in
      Wserver.printf "<head><title>";
      title true;
      Wserver.printf "</title>\n<body>\n<h1>";
      title false;
      Wserver.printf "</body>\n");
  raise Exit

let purge_who tm xcl sec =
  let sec = float sec in
  let to_remove =
    W.fold
      (fun k who l ->
         match who.acc_times with
           tm0 :: _ -> if tm -. tm0 > sec then k :: l else l
         | [] -> k :: l)
      xcl.who []
  in
  List.iter (fun k -> xcl.who <- W.remove k xcl.who) to_remove

let input_excl ic =
  let b = really_input_string ic (String.length magic_robot) in
  if b <> magic_robot then raise Not_found else (input_value ic : excl)

let output_excl oc xcl =
  output_string oc magic_robot; output_value oc (xcl : excl)

let robot_excl () =
  let fname = SrcfileDisplay.adm_file "robot" in
  let xcl =
    match try Some (Secure.open_in_bin fname) with _ -> None with
      Some ic ->
        let v =
          try input_excl ic with
            _ -> {excl = []; who = W.empty; max_conn = 0, ""}
        in
        close_in ic; v
    | None -> {excl = []; who = W.empty; max_conn = 0, ""}
  in
  xcl, fname

let min_disp_req = ref 6

let check oc_opt tm from max_call sec conf suicide =
  let nfw =
    if conf.wizard then Wizard conf.user
    else if conf.friend then Friend conf.user
    else Normal
  in
  let (xcl, fname) = robot_excl () in
  let refused =
    match try Some (List.assoc from xcl.excl) with Not_found -> None with
      Some att ->
        incr att;
        begin match !att mod max_call, oc_opt with
          0, Some oc ->
            Util.fprintf_date oc (Unix.localtime tm);
            Printf.fprintf oc "\n";
            Printf.fprintf oc "  From: %s\n" from;
            Printf.fprintf oc "  %d refused attempts;" !att;
            Printf.fprintf oc " to restore access, delete file \"%s\"\n" fname
        | _ -> ()
        end;
        true
    | None ->
        purge_who tm xcl sec;
        let r = try (W.find from xcl.who).acc_times with Not_found -> [] in
        let (cnt, tml, tm0) =
          let sec = float sec in
          let rec count cnt tml =
            function
              [] -> cnt, tml, tm
            | [tm1] ->
                if tm -. tm1 < sec then cnt + 1, tm1 :: tml, tm1
                else cnt, tml, tm1
            | tm1 :: tml1 ->
                if tm -. tm1 < sec then count (cnt + 1) (tm1 :: tml) tml1
                else cnt, tml, tm1
          in
          count 1 [] r
        in
        let r = List.rev tml in
        xcl.who <-
          W.add from
            {acc_times = tm :: r; oldest_time = tm0; nb_connect = cnt;
             nbase = conf.bname; utype = nfw}
            xcl.who;
        let refused =
          if suicide || cnt > max_call then
            begin
              begin match oc_opt with
                Some oc ->
                  Printf.fprintf oc "--- %s is a robot" from;
                  if suicide then
                    Printf.fprintf oc " (called the \"suicide\" request)\n"
                  else
                    Printf.fprintf oc " (%d > %d connections in %g <= %d seconds)\n"
                      cnt max_call (tm -. tm0) sec
              | None -> ()
              end;
              xcl.excl <- (from, ref 1) :: xcl.excl;
              xcl.who <- W.remove from xcl.who;
              xcl.max_conn <- 0, "";
              true
            end
          else false
        in
        begin match xcl.excl, oc_opt with
          [_; _], Some oc ->
            List.iter
              (fun (s, att) ->
                 Printf.fprintf oc "--- excluded:";
                 Printf.fprintf oc " %s (%d refused attempts)\n" s !att;
                 ())
              xcl.excl;
            Printf.fprintf oc "--- to restore access, delete file \"%s\"\n" fname
        | _ -> ()
        end;
        let (list, nconn) =
          W.fold
            (fun k w (list, nconn) ->
               let tm = w.oldest_time in
               let nb = w.nb_connect in
               if nb > fst xcl.max_conn then xcl.max_conn <- nb, k;
               (if nb < !min_disp_req then list else (k, tm, nb) :: list),
               nconn + 1)
            xcl.who ([], 0)
        in
        let list =
          List.sort
            (fun (_, tm1, nb1) (_, tm2, nb2) ->
               match compare nb2 nb1 with
                 0 -> compare tm2 tm1
               | x -> x)
            list
        in
        begin match oc_opt with
          Some oc ->
            List.iter
              (fun (k, tm0, nb) ->
                 Printf.fprintf oc "--- %3d req - %3.0f sec - %s\n" nb (tm -. tm0) k)
              list;
            Printf.fprintf oc "--- max %d req by %s / conn %d\n" (fst xcl.max_conn)
              (snd xcl.max_conn) nconn
        | None -> ()
        end;
        refused
  in
  begin match
    (try Some (Secure.open_out_bin fname) with Sys_error _ -> None)
  with
    Some oc -> output_excl oc xcl; close_out oc
  | None -> ()
  end;
  if refused then robot_error conf max_call sec;
  W.fold
    (fun _ w (c, cw, cf, wl) ->
       if w.nbase = conf.bname && w.nbase <> "" then
         match w.utype with
           Wizard n ->
             let at = List.hd w.acc_times in
             if List.mem_assoc n wl then
               let old_at = List.assoc n wl in
               if at > old_at then
                 let wl = List.remove_assoc n wl in c, cw, cf, (n, at) :: wl
               else c, cw, cf, wl
             else c + 1, cw + 1, cf, (n, at) :: wl
         | Friend _ ->
             if w.nb_connect > 2 then c + 1, cw, cf + 1, wl else c, cw, cf, wl
         | Normal ->
             if w.nb_connect > 2 then c + 1, cw, cf, wl else c, cw, cf, wl
       else c, cw, cf, wl)
    xcl.who (0, 0, 0, [])

(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config

let src = Logs.Src.create ~doc:"Robot" "ROB "

module Log = (val Logs.src_log src : Logs.LOG)
module Code = Geneweb_http.Code

(* Bump: `who`'s key changed from `ip` to `(bname, ip)`, the `nbase`
   field was dropped from `who` (now redundant with the map key), and
   `max_conn` changed from `int * string` to `int * (string * string)`.
   Reading an old "GWRB0008" file with these new types via `input_value`
   would silently misinterpret bytes rather than raise cleanly, so we
   bump the magic and let it fall into the "unrecognized format" reset
   path below instead of attempting a field-by-field migration - this
   is a transient anti-abuse cache, not data worth the migration risk. *)
let magic_robot = "GWRB0009"

(* The robot-ban state below is deliberately GLOBAL: an IP must be
   blocked across every database served by this gwd instance, not just
   the one it happens to be hitting right now. The lock guarding access
   to it must therefore also be global, and must NOT go through
   GWPARAM.adm_file: in "reorg" mode, adm_file resolves to a per-base
   directory (bname.gwb/config/cnt/...), which would let two workers
   serving two different bases take two different locks while both
   read-modify-write this same shared file. *)
let robot_dir () = String.concat Filename.dir_sep [ Secure.base_dir (); "cnt" ]
let lock_file () = Filename.concat (robot_dir ()) "gwd_robot.lck"

(* Detection window is scoped per (base, ip): a robot's signature is
   usually excessive traffic against one specific database, and this
   avoids false positives from legitimate visitors browsing several
   bases on the same multi-base site. *)
module W = Map.Make (struct
  type t = string * string (* (bname, ip) *)

  let compare = compare
end)

type norfriwiz = Normal | Friend of string | Wizard of string

type who = {
  acc_times : float list;
  oldest_time : float;
  nb_connect : int;
  utype : norfriwiz;
}

type excl = {
  mutable excl : (string * int ref) list;
  mutable who : who W.t;
  mutable max_conn : int * (string * string); (* (count, (bname, ip)) *)
  mutable last_summary : float;
}

let is_log_worthy n =
  let rec check_power10 base =
    if base > n then false
    else n = base || n = base * 2 || n = base * 5 || check_power10 (base * 10)
  in
  n > 0 && check_power10 1

let is_valid_ip_part s =
  try
    let n = int_of_string s in
    n >= 0 && n <= 255
  with _ -> s = "*"

let ip_matches_pattern ip pattern =
  if String.contains pattern '*' then
    let pattern_parts = String.split_on_char '.' pattern in
    let ip_parts = String.split_on_char '.' ip in
    if List.length ip_parts <> 4 then false
    else if not (List.for_all is_valid_ip_part ip_parts) then false
    else
      let rec match_parts pp ip_p =
        match (pp, ip_p) with
        | [], [] -> true
        | "*" :: _, _ -> true
        | p :: pt, i :: it when p = i -> match_parts pt it
        | _ -> false
      in
      match_parts pattern_parts ip_parts
  else ip = pattern

let is_ip_already_covered excl_list ip =
  List.exists (fun (pattern, _) -> ip_matches_pattern ip pattern) excl_list

let robot_error conf cnt sec =
  Output.status conf Code.Forbidden;
  Output.header conf "Content-type: text/html; charset=iso-8859-1";
  let env =
    Templ.Env.(
      empty
      |> add "cnt" (Templ.Vstring (Adef.encoded @@ string_of_int cnt))
      |> add "sec" (Templ.Vstring (Adef.encoded @@ string_of_int sec)))
  in
  (try Templ.output_simple conf env "robot"
   with _ ->
     let title _ = Output.print_sstring conf "Access refused" in
     Output.print_sstring conf "<head><title>";
     title true;
     Output.print_sstring conf "</title>\n<body>\n<h1>";
     title false;
     Output.print_sstring conf "</body>\n");
  raise Exit

let purge_who tm xcl sec =
  let sec = float sec in
  let to_remove =
    W.fold
      (fun k who l ->
        match who.acc_times with
        | tm0 :: _ -> if tm -. tm0 > sec then k :: l else l
        | [] -> k :: l)
      xcl.who []
  in
  List.iter (fun k -> xcl.who <- W.remove k xcl.who) to_remove

let output_excl oc xcl =
  output_string oc magic_robot;
  output_value oc (xcl : excl)

let save xcl fname =
  match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
      output_excl oc xcl;
      close_out oc
  | None -> ()

let robot_excl () =
  let fname = Filename.concat (robot_dir ()) "robot" in
  let xcl =
    match try Some (Secure.open_in_bin fname) with _ -> None with
    | Some ic -> (
        try
          let b = really_input_string ic (String.length magic_robot) in
          if b = magic_robot then (
            let v = (input_value ic : excl) in
            close_in ic;
            v)
          else (
            close_in ic;
            {
              excl = [];
              who = W.empty;
              max_conn = (0, ("", ""));
              last_summary = 0.0;
            })
        with _ ->
          close_in ic;
          {
            excl = [];
            who = W.empty;
            max_conn = (0, ("", ""));
            last_summary = 0.0;
          })
    | None ->
        {
          excl = [];
          who = W.empty;
          max_conn = (0, ("", ""));
          last_summary = 0.0;
        }
  in
  (xcl, fname)

let min_disp_req = ref 6

let log_summary tm xcl nconn =
  let local_tm = Unix.localtime tm in
  Log.info (fun k ->
      k "%s === ROBOT SUMMARY ===" (Mutil.sprintf_date local_tm :> string));
  Log.info (fun k ->
      k "  Blocked IPs: %d, Monitored: %d" (List.length xcl.excl) nconn);
  let max_nb, (max_bname, max_ip) = xcl.max_conn in
  Log.info (fun k ->
      k "  Most active: %d req by %s on %s" max_nb max_ip
        (if max_bname = "" then "(no base)" else max_bname));
  Log.info (fun k -> k "  Blocked robots detail:");
  List.iter
    (fun (ip, att) -> Logs.info (fun k -> k "    %s: %d attempts" ip !att))
    ( List.rev xcl.excl |> fun l ->
      let rec take n = function
        | [] -> []
        | h :: t -> if n = 0 then [] else h :: take (n - 1) t
      in
      take 20 l );
  if List.length xcl.excl > 20 then
    Log.info (fun k -> k "    ... and %d more" (List.length xcl.excl - 20))

let check tm from max_call sec conf suicide =
  let nfw =
    if conf.wizard then Wizard conf.user
    else if conf.friend then Friend conf.user
    else Normal
  in
  let xcl, fname = robot_excl () in
  let key = (conf.bname, from) in
  let refused =
    match
      try
        Some
          (List.find
             (fun (pattern, _) -> ip_matches_pattern from pattern)
             xcl.excl
          |> snd)
      with Not_found -> None
    with
    | Some att ->
        incr att;
        if is_log_worthy !att then
          Log.info (fun k -> k "ROBOT %s: %d refused attempts" from !att);
        true
    | None ->
        purge_who tm xcl sec;
        let r = try (W.find key xcl.who).acc_times with Not_found -> [] in
        let cnt, tml, tm0 =
          let sec = float sec in
          let rec count cnt tml = function
            | [] -> (cnt, tml, tm)
            | [ tm1 ] ->
                if tm -. tm1 < sec then (cnt + 1, tm1 :: tml, tm1)
                else (cnt, tml, tm1)
            | tm1 :: tml1 ->
                if tm -. tm1 < sec then count (cnt + 1) (tm1 :: tml) tml1
                else (cnt, tml, tm1)
          in
          count 1 [] r
        in
        let r = List.rev tml in
        xcl.who <-
          W.add key
            {
              acc_times = tm :: r;
              oldest_time = tm0;
              nb_connect = cnt;
              utype = nfw;
            }
            xcl.who;
        let refused =
          if suicide || cnt > max_call then (
            Log.info (fun k ->
                k "ROBOT %s: BLOCKED after %d req in %.0fs%s\n" from cnt
                  (tm -. tm0)
                  (if suicide then " (suicide)" else ""));
            if not (is_ip_already_covered xcl.excl from) then
              xcl.excl <- (from, ref 1) :: xcl.excl
            else
              Log.info (fun k ->
                  k "ROBOT %s: BLOCKED (covered by existing pattern)\n" from);
            xcl.who <- W.remove key xcl.who;
            xcl.max_conn <- (0, ("", ""));
            true)
          else false
        in
        let _list, nconn =
          W.fold
            (fun k w (list, nconn) ->
              let tm = w.oldest_time in
              let nb = w.nb_connect in
              if nb > fst xcl.max_conn then xcl.max_conn <- (nb, k);
              ( (if nb < !min_disp_req then list else (k, tm, nb) :: list),
                nconn + 1 ))
            xcl.who ([], 0)
        in
        let four_hours = 4.0 *. 3600.0 in
        if tm -. xcl.last_summary > four_hours then (
          xcl.last_summary <- tm;
          log_summary tm xcl nconn);
        refused
  in
  save xcl fname;
  if refused then robot_error conf max_call sec;
  W.fold
    (fun (bname, _ip) w (c, cw, cf, wl) ->
      if bname = conf.bname && bname <> "" then
        match w.utype with
        | Wizard n ->
            let at = List.hd w.acc_times in
            if List.mem_assoc n wl then
              let old_at = List.assoc n wl in
              if at > old_at then
                let wl = List.remove_assoc n wl in
                (c, cw, cf, (n, at) :: wl)
              else (c, cw, cf, wl)
            else (c + 1, cw + 1, cf, (n, at) :: wl)
        | Friend _ ->
            if w.nb_connect > 2 then (c + 1, cw, cf + 1, wl) else (c, cw, cf, wl)
        | Normal ->
            if w.nb_connect > 2 then (c + 1, cw, cf, wl) else (c, cw, cf, wl)
      else (c, cw, cf, wl))
    xcl.who (0, 0, 0, [])

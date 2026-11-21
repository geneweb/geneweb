(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Config

let src = Logs.Src.create ~doc:"Robot" "ROB "

module Log = (val Logs.src_log src : Logs.LOG)
module Code = Geneweb_http.Code
include Geneweb_robot

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

let robot_excl () =
  let fname = file () in
  let xcl =
    match try Some (Secure.open_in_bin fname) with _ -> None with
    | Some ic -> (
        try
          let b = really_input_string ic (String.length magic_robot) in
          if b = magic_robot then (
            let v = (input_value ic : excl) in
            close_in ic;
            v)
          else if b = "GWRB0007" then (
            let old_data =
              (input_value ic
                : (string * int ref) list * who W.t * (int * string))
            in
            close_in ic;
            let excl, who, max_conn = old_data in
            let new_data = { excl; who; max_conn; last_summary = 0.0 } in
            (try
               let oc = open_out_bin fname in
               output_excl oc new_data;
               close_out oc
             with _ -> ());
            new_data)
          else (
            close_in ic;
            empty ())
        with _ ->
          close_in ic;
          empty ())
    | None -> empty ()
  in
  (xcl, fname)

let summary () =
  let xcl, _ = robot_excl () in
  let n, ip = xcl.max_conn in
  Printf.sprintf "%d blocked, %d monitored%s" (List.length xcl.excl)
    (nb_ips xcl.who)
    (if n > 0 then Printf.sprintf ", most active %s (%d req.)" ip n else "")

(* Users connected to the base of [conf], from the clients recorded in [xcl]
   within the last [sec] seconds. *)
let connections conf sec xcl =
  (* keep, for each user name, the time of their most recent access *)
  let add_named n at l =
    match List.assoc_opt n l with
    | Some old_at -> if at > old_at then (n, at) :: List.remove_assoc n l else l
    | None -> (n, at) :: l
  in
  let c, cw, cf, wl, fl =
    W.fold
      (fun _ w (c, cw, cf, wl, fl) ->
        if w.nbase = conf.bname && w.nbase <> "" then
          match w.utype with
          | Wizard n ->
              let at = List.hd w.acc_times in
              if List.mem_assoc n wl then (c, cw, cf, add_named n at wl, fl)
              else (c + 1, cw + 1, cf, (n, at) :: wl, fl)
          | Friend n ->
              if w.nb_connect > 2 then
                let fl =
                  if n = "" then fl else add_named n (List.hd w.acc_times) fl
                in
                (c + 1, cw, cf + 1, wl, fl)
              else (c, cw, cf, wl, fl)
          | Normal ->
              if w.nb_connect > 2 then (c + 1, cw, cf, wl, fl)
              else (c, cw, cf, wl, fl)
        else (c, cw, cf, wl, fl))
      xcl.who (0, 0, 0, [], [])
  in
  {
    Config.cn_total = c;
    cn_wizards = cw;
    cn_friends = cf;
    cn_wizard_list = wl;
    cn_friend_list = fl;
    cn_window = sec;
  }

let check tm from max_call sec conf suicide =
  let nfw =
    if conf.wizard then Wizard conf.user
    else if conf.friend then Friend conf.user
    else Normal
  in
  let xcl, fname = robot_excl () in
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
        let key = who_key from nfw in
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
              nbase = conf.bname;
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
            xcl.who <- remove_ip from xcl.who;
            xcl.max_conn <- (0, "");
            true)
          else false
        in
        W.iter
          (fun k w ->
            if w.nb_connect > fst xcl.max_conn then
              xcl.max_conn <- (w.nb_connect, k))
          xcl.who;
        refused
  in
  (match try Some (Secure.open_out_bin fname) with Sys_error _ -> None with
  | Some oc ->
      output_excl oc xcl;
      close_out oc
  | None -> ());
  if refused then robot_error conf max_call sec;
  connections conf sec xcl

let snapshot conf sec =
  let xcl, _ = robot_excl () in
  purge_who (Unix.time ()) xcl sec;
  connections conf sec xcl

let addresses conf sec =
  let xcl, _ = robot_excl () in
  purge_who (Unix.time ()) xcl sec;
  let ips, _ =
    W.fold
      (fun k w ((ips, wl) as acc) ->
        if w.nbase = conf.bname && w.nbase <> "" then
          match w.utype with
          | Wizard n ->
              if List.mem n wl then acc else (ip_of_key k :: ips, n :: wl)
          | Friend _ | Normal ->
              if w.nb_connect > 2 then (ip_of_key k :: ips, wl) else acc
        else acc)
      xcl.who ([], [])
  in
  ips

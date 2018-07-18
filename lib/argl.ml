(* $Id: argl.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Printf

let action_arg s sl =
  function
    Arg.Set r -> if s = "" then begin r := true; Some sl end else None
  | Arg.Clear r -> if s = "" then begin r := false; Some sl end else None
  | Arg.Rest f -> List.iter f (s :: sl); Some []
  | Arg.String f ->
      if s = "" then
        match sl with
          s :: sl -> f s; Some sl
        | [] -> None
      else begin f s; Some sl end
  | Arg.Int f ->
      if s = "" then
        match sl with
          s :: sl -> (try f (int_of_string s); Some sl with Failure _ -> None)
        | [] -> None
      else (try f (int_of_string s); Some sl with Failure _ -> None)
  | Arg.Float f ->
      if s = "" then
        match sl with
          s :: sl -> f (float_of_string s); Some sl
        | [] -> None
      else begin f (float_of_string s); Some sl end
  | Arg.Unit f -> if s = "" then begin f (); Some sl end else None
  | Arg.Set_string r ->
      if s = "" then
        match sl with
          s :: sl -> r := s; Some sl
        | [] -> None
      else begin r := s; Some sl end
  | Arg.Set_int r ->
      if s = "" then
        match sl with
          s :: sl ->
            (try r := int_of_string s; Some sl with Failure _ -> None)
        | [] -> None
      else (try r := int_of_string s; Some sl with Failure _ -> None)
  | Arg.Set_float r ->
      if s = "" then
        match sl with
          s :: sl -> r := float_of_string s; Some sl
        | [] -> None
      else begin r := float_of_string s; Some sl end
  | Arg.Symbol (syms, f) ->
      begin match if s = "" then sl else s :: sl with
        s :: sl when List.mem s syms -> f s; Some sl
      | _ -> None
      end
  | _ -> assert false

let common_start s1 s2 =
  let rec loop i =
    if i = String.length s1 || i = String.length s2 then i
    else if s1.[i] = s2.[i] then loop (i + 1)
    else i
  in
  loop 0

let rec parse_arg s sl =
  function
    (name, action, _) :: spec_list ->
      let i = common_start s name in
      if i = String.length name then
        try action_arg (String.sub s i (String.length s - i)) sl action with
          Arg.Bad _ -> parse_arg s sl spec_list
      else parse_arg s sl spec_list
  | [] -> None

let rec parse_aux spec_list anon_fun =
  function
    [] -> []
  | s :: sl ->
      if String.length s > 1 && s.[0] = '-' then
        match parse_arg s sl spec_list with
          Some sl -> parse_aux spec_list anon_fun sl
        | None -> s :: parse_aux spec_list anon_fun sl
      else begin (anon_fun s : unit); parse_aux spec_list anon_fun sl end

let parse_arg_list spec_list anon_fun remaining_args =
  let spec_list =
    List.sort (fun (k1, _, _) (k2, _, _) -> compare k2 k1) spec_list
  in
  try parse_aux spec_list anon_fun remaining_args with
    Arg.Bad s ->
      eprintf "Error: %s\n" s;
      eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2

let usage speclist errmsg =
  printf "%s\n" errmsg;
  List.iter (fun (key, _, doc) -> printf "  %s %s\n" key doc) speclist;
  flush stdout

let parse_list spec_list anonfun errmsg list =
  match parse_arg_list spec_list anonfun list with
    [] -> ()
  | "-help" :: sl -> usage spec_list errmsg; exit 0
  | s :: sl ->
      eprintf "%s: unknown or misused option\n" s;
      eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2

let parse spec_list anonfun errmsg =
  let remaining_args =
    let rec loop l i =
      if i = Array.length Sys.argv then l
      else
        let s =
          let s = Sys.argv.(i) in
          let len = String.length s in
          if len > 2 && s.[0] = '"' && s.[len-1] = '"' then
            String.sub s 1 (len - 2)
          else s
        in
        loop (s :: l) (i + 1)
    in
    List.rev (loop [] (!(Arg.current) + 1))
  in
  parse_list spec_list anonfun errmsg remaining_args

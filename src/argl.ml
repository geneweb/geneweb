(* $Id: argl.ml,v 4.1 2001-04-18 09:38:33 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

value action_arg s sl =
  fun
  [ Arg.Unit f -> if s = "" then do { f (); Some sl } else None
  | Arg.Set r -> if s = "" then do { r.val := True; Some sl } else None
  | Arg.Clear r -> if s = "" then do { r.val := False; Some sl } else None
  | Arg.Rest f -> do { List.iter f [s :: sl]; Some [] }
  | Arg.String f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f s; Some sl }
        | [] -> None ]
      else do { f s; Some sl }
  | Arg.Int f ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do { f (int_of_string s); Some sl } with
            [ Failure "int_of_string" -> None ]
        | [] -> None ]
      else
        try do { f (int_of_string s); Some sl } with
        [ Failure "int_of_string" -> None ]
  | Arg.Float f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f (float_of_string s); Some sl }
        | [] -> None ]
      else do { f (float_of_string s); Some sl } ]
;

value common_start s1 s2 =
  loop 0 where rec loop i =
    if i == String.length s1 || i == String.length s2 then i
    else if s1.[i] == s2.[i] then loop (i + 1)
    else i
;

value rec parse_arg s sl =
  fun
  [ [(name, action, _) :: spec_list] ->
      let i = common_start s name in
      if i == String.length name then
        try action_arg (String.sub s i (String.length s - i)) sl action with
        [ Arg.Bad _ -> parse_arg s sl spec_list ]
      else parse_arg s sl spec_list
  | [] -> None ]
;

value rec parse_aux spec_list anon_fun =
  fun
  [ [] -> []
  | [s :: sl] ->
      if String.length s > 1 && s.[0] = '-' then
        match parse_arg s sl spec_list with
        [ Some sl -> parse_aux spec_list anon_fun sl
        | None -> [s :: parse_aux spec_list anon_fun sl] ]
      else do { (anon_fun s : unit); parse_aux spec_list anon_fun sl } ]
;

value parse_arg_list spec_list anon_fun remaining_args =
  let spec_list =
    Sort.list (fun (k1, _, _) (k2, _, _) -> k1 >= k2) spec_list
  in
  try parse_aux spec_list anon_fun remaining_args with
  [ Arg.Bad s ->
      do {
        Printf.eprintf "Error: %s\n" s;
        Printf.eprintf "Use option -help for usage\n";
        flush stderr;
        exit 2
      } ]
;

value usage speclist errmsg =
  do {
    Printf.printf "%s\n" errmsg;
    List.iter (fun (key, _, doc) -> Printf.printf "  %s %s\n" key doc)
      speclist;
    flush stdout;
  }
;

value parse_list spec_list anonfun errmsg list =
  match parse_arg_list spec_list anonfun list with
  [ [] -> ()
  | ["-help" :: sl] -> do { usage spec_list errmsg; exit 0 }
  | [s :: sl] ->
      do {
        Printf.eprintf "%s: unknown or misused option\n" s;
        Printf.eprintf "Use option -help for usage\n";
        flush stderr;
        exit 2
      } ]
;

value parse spec_list anonfun errmsg =
  let remaining_args =
    List.rev (loop [] (Arg.current.val + 1)) where rec loop l i =
      if i == Array.length Sys.argv then l
      else loop [Sys.argv.(i) :: l] (i + 1)
  in
  parse_list spec_list anonfun errmsg remaining_args
;

(* $Id: argl.ml,v 1.1 1998-09-01 14:32:02 ddr Exp $ *)

value action_arg s sl =
  fun
  [ Arg.Unit f -> if s = "" then do f (); return Some sl else None
  | Arg.Set r -> if s = "" then do r.val := True; return Some sl else None
  | Arg.Clear r -> if s = "" then do r.val := False; return Some sl else None
  | Arg.Rest f -> do List.iter f [s :: sl]; return Some []
  | Arg.String f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do f s; return Some sl
        | [] -> None ]
      else do f s; return Some sl
  | Arg.Int f ->
      if s = "" then
        match sl with
        [ [s :: sl] ->
            try do f (int_of_string s); return Some sl with
            [ Failure "int_of_string" -> None ]
        | [] -> None ]
      else
        try do f (int_of_string s); return Some sl with
        [ Failure "int_of_string" -> None ]
  | Arg.Float f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do f (float_of_string s); return Some sl
        | [] -> None ]
      else do f (float_of_string s); return Some sl ]
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
      else do anon_fun s; return parse_aux spec_list anon_fun sl ]
;

value parse_arg_list spec_list anon_fun remaining_args =
  try parse_aux spec_list anon_fun remaining_args with
  [ Arg.Bad s ->
      do Printf.eprintf "Error: %s\n" s;
         Printf.eprintf "Use option -help for usage\n";
         flush stderr;
      return exit 2 ]
;

value usage speclist errmsg =
  do Printf.printf "%s\n" errmsg;
     List.iter (fun (key, _, doc) -> Printf.printf "  %s %s\n" key doc)
       speclist;
     flush stdout;
  return ()
;

value parse_list spec_list anonfun errmsg list =
  do match parse_arg_list spec_list anonfun list with
     [ [] -> ()
     | ["-help" :: sl] ->
         do usage spec_list errmsg; return exit 0
     | [s :: sl] ->
         do Printf.eprintf "%s: unknown or misused option\n" s;
            Printf.eprintf "Use option -help for usage\n";
            flush stderr;
         return exit 2 ];
  return ()
;

value parse spec_list anonfun errmsg =
  let remaining_args =
    List.rev (loop [] (Arg.current.val + 1)) where rec loop l i =
      if i == Array.length Sys.argv then l
      else loop [Sys.argv.(i) :: l] (i + 1)
  in
  parse_list spec_list anonfun errmsg remaining_args
;

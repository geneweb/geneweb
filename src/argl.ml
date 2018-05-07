(* $Id: argl.ml,v 5.3 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Printf;

value action_arg s sl =
  fun
  [ Arg.Set r -> if s = "" then do { r.val := True; Some sl } else None
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
            [ Failure _ -> None ]
        | [] -> None ]
      else
        try do { f (int_of_string s); Some sl } with
        [ Failure _ -> None ]
  | Arg.Float f ->
      if s = "" then
        match sl with
        [ [s :: sl] -> do { f (float_of_string s); Some sl }
        | [] -> None ]
      else do { f (float_of_string s); Some sl }
  | x ->
      IFDEF OCAML_307 THEN
        match x with
        [ Arg.Unit f -> if s = "" then do { f (); Some sl } else None
        | Arg.Set_string r ->
            if s = "" then
              match sl with
              [ [s :: sl] -> do { r.val := s; Some sl }
              | [] -> None ]
            else do { r.val := s; Some sl }
        | Arg.Set_int r ->
            if s = "" then
              match sl with
              [ [s :: sl] ->
                  try do { r.val := (int_of_string s); Some sl } with
                  [ Failure "int_of_string" -> None ]
              | [] -> None ]
            else
              try do { r.val := (int_of_string s); Some sl } with
              [ Failure "int_of_string" -> None ]
        | Arg.Set_float r ->
            if s = "" then
              match sl with
              [ [s :: sl] -> do { r.val := (float_of_string s); Some sl }
              | [] -> None ]
            else do { r.val := (float_of_string s); Some sl }
        | Arg.Symbol syms f ->
            match (if s = "" then sl else [s :: sl]) with
            [ [s :: sl] when List.mem s syms -> do { f s; Some sl }
            | _ -> None ]
        | x -> assert False ]
      ELSE
        match x with
        [ Arg.Unit f -> if s = "" then do { f (); Some sl } else None
        | x -> assert False ]
      END ]
;

value common_start s1 s2 =
  loop 0 where rec loop i =
    if i = String.length s1 || i = String.length s2 then i
    else if s1.[i] = s2.[i] then loop (i + 1)
    else i
;

value rec parse_arg s sl =
  fun
  [ [(name, action, _) :: spec_list] ->
      let i = common_start s name in
      if i = String.length name then
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
    List.sort (fun (k1, _, _) (k2, _, _) -> compare k2 k1) spec_list
  in
  try parse_aux spec_list anon_fun remaining_args with
  [ Arg.Bad s ->
      do {
        eprintf "Error: %s\n" s;
        eprintf "Use option -help for usage\n";
        flush stderr;
        exit 2
      } ]
;

value usage speclist errmsg =
  do {
    printf "%s\n" errmsg;
    List.iter (fun (key, _, doc) -> printf "  %s %s\n" key doc)
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
        eprintf "%s: unknown or misused option\n" s;
        eprintf "Use option -help for usage\n";
        flush stderr;
        exit 2
      } ]
;

value parse spec_list anonfun errmsg =
  let remaining_args =
    List.rev (loop [] (Arg.current.val + 1)) where rec loop l i =
      if i = Array.length Sys.argv then l
      else
        let s =
          let s = Sys.argv.(i) in
          let len = String.length s in
          if len > 2 && s.[0] = '"' && s.[len-1] = '"' then
            String.sub s 1 (len-2)
          else s
        in
        loop [s :: l] (i + 1)
  in
  parse_list spec_list anonfun errmsg remaining_args
;

let rec cut_at_equal i s =
  if i = String.length s then (s, "")
  else if s.[i] = '=' then
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  else cut_at_equal (succ i) s

let strip_trailing_spaces s =
  let len =
    let rec loop len =
      if len = 0 then 0
      else
        match s.[len - 1] with
        | ' ' | '\n' | '\r' | '\t' -> loop (len - 1)
        | _ -> len
    in
    loop (String.length s)
  in
  String.sub s 0 len

let read_base_env bname =
  let fname = Geneweb.Util.bpath (bname ^ ".gwf") in
  try
    let ic = Secure.open_in fname in
    let env =
      let rec loop env =
        match input_line ic with
        | s ->
            let s = strip_trailing_spaces s in
            if s = "" || s.[0] = '#' then loop env
            else loop (cut_at_equal 0 s :: env)
        | exception End_of_file -> env
      in
      loop []
    in
    close_in ic;
    env
  with Sys_error _ -> []

let get_private_years base_env =
  try int_of_string (List.assoc "private_years" base_env)
  with Not_found | Failure _ -> 150

let get_default_contemporary_private_years base_env =
  try int_of_string (List.assoc "default_contemporary_private_years" base_env)
  with _ -> 100

let allowed_denied_titles key extra_line env base_env () =
  if Geneweb.Util.p_getenv env "all_titles" = Some "on" then []
  else
    try
      let fname = List.assoc key base_env in
      if fname = "" then []
      else
        let ic = Secure.open_in (Filename.concat (Secure.base_dir ()) fname) in
        let rec loop set =
          let line, eof =
            try (input_line ic, false) with End_of_file -> ("", true)
          in
          let set =
            let line = if eof then extra_line |> Mutil.decode else line in
            if line = "" || line.[0] = ' ' || line.[0] = '#' then set
            else
              let line =
                match String.index_opt line '/' with
                | Some i ->
                    let len = String.length line in
                    let tit = String.sub line 0 i in
                    let pla = String.sub line (i + 1) (len - i - 1) in
                    (if tit = "*" then tit else Name.lower tit)
                    ^ "/"
                    ^ if pla = "*" then pla else Name.lower pla
                | None -> Name.lower line
              in
              Mutil.StrSet.add line set
          in
          if eof then (
            close_in ic;
            Mutil.StrSet.elements set)
          else loop set
        in
        loop Mutil.StrSet.empty
    with Not_found | Sys_error _ -> []

let allowed_titles env =
  let extra_line =
    try List.assoc "extra_title" env with Not_found -> Adef.encoded ""
  in
  allowed_denied_titles "allowed_titles_file" extra_line env

let denied_titles = allowed_denied_titles "denied_titles_file" (Adef.encoded "")

let get_public_if_titles base_env =
  try List.assoc "public_if_titles" base_env = "yes" with Not_found -> false

let get_public_if_no_date base_env =
  try List.assoc "public_if_no_date" base_env = "yes" with Not_found -> false

let get_hide_private_names base_env =
  try List.assoc "hide_private_names" base_env = "yes" with Not_found -> false

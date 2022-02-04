open Geneweb

let ns = "no_index"

let url_no_index conf base pwd =
  let scratch s = Mutil.encode (Name.lower (Gwdb.sou base s)) in
  let get_a_person v =
    try
      let i = Gwdb.iper_of_string v in
      let p = Util.pget conf base i in
      if Util.is_hide_names conf p
      && not (Util.authorized_age conf base p)
      || Util.is_hidden p
      then
        None
      else
        let f = scratch (Gwdb.get_first_name p) in
        let s = scratch (Gwdb.get_surname p) in
        let oc = string_of_int (Gwdb.get_occ p) in Some (f, s, oc)
    with Failure _ -> None
  in
  let get_a_family v =
    try
      let i = Gwdb.ifam_of_string v in
      let fam = Gwdb.foi base i in
      let p = Util.pget conf base (Gwdb.get_father fam) in
      let f = scratch (Gwdb.get_first_name p) in
      let s = scratch (Gwdb.get_surname p) in
      if f = "" || s = "" then None
      else
        let oc = string_of_int (Gwdb.get_occ p) in
        let u = Util.pget conf base (Gwdb.get_father fam) in
        let n =
          let rec loop k =
            if (Gwdb.get_family u).(k) = i then
              string_of_int k
            else loop (k + 1)
          in
          loop 0
        in
        Some (f, s, oc, n)
    with Failure _ -> None
  in
  let env =
    let rec loop =
      function
        [] -> []
      | ("opt", "no_index") :: l -> loop l
      | ("opt", "no_index_pwd") :: l -> loop l
      | (("dsrc" | "escache" | "templ"), _) :: l -> loop l
      | ("i", v) :: l -> new_env "i" v (fun x -> x) l
      | ("ei", v) :: l -> new_env "ei" v (fun x -> "e" ^ x) l
      | (k, v) :: l when String.length k = 2 && k.[0] = 'i' ->
        let c = String.make 1 k.[1] in new_env k v (fun x -> x ^ c) l
      | (k, v) :: l when String.length k > 2 && k.[0] = 'e' && k.[1] = 'f' ->
        new_fam_env k v (fun x -> x ^ k) l
      | kv :: l -> kv :: loop l
    and new_env k v c l =
      match get_a_person v with
        Some (f, s, oc) ->
        if oc = "0" then (c "p", f) :: (c "n", s) :: loop l
        else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: loop l
      | None -> (k, v) :: loop l
    and new_fam_env k v c l =
      match get_a_family v with
        Some (f, s, oc, n) ->
        let l = loop l in
        let l = if n = "0" then l else (c "f", n) :: l in
        if oc = "0" then (c "p", f) :: (c "n", s) :: l
        else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: l
      | None -> (k, v) :: loop l
    in
    loop conf.env
  in
  let addr =
    let pref =
      let s = Util.get_request_string conf in
      match String.rindex_opt s '?' with
      | Some i -> String.sub s 0 i
      | None -> s
    in
    let pref =
      if pwd then pref
      else
        match String.rindex_opt pref '_' with
        | Some i -> String.sub pref 0 i
        | None -> pref
    in
    Util.get_server_string conf ^ pref
  in
  let suff =
    List.fold_right
      (fun (x, v) s ->
         if v != "" then
           let sep = if s = "" then "" else "&" in x ^ "=" ^ v ^ sep ^ s
         else s )
      (("lang", conf.lang) :: env) ""
  in
  if conf.cgi then addr ^ "?b=" ^ conf.bname ^ "&" ^ suff
  else addr ^ "?" ^ suff

let () =
  Gwd_lib.GwdPlugin.register_se ~ns @@ fun _assets conf -> function
  | Some base ->
    let opt1 = Util.p_getenv conf.env "opt" = Some "no_index" in
    let opt2 = Util.p_getenv conf.env "opt" = Some "no_index_pwd" in
    if opt1 || opt2
    then begin
      let link = url_no_index conf base opt2 in
      Output.printf conf "<a href=\"http://%s\">%s</a>" link link;
      Output.flush conf ;
      exit 0
    end
  | None ->
    Hutil.incorrect_request conf

open Config
module Driver = Geneweb_db.Driver

let drop (k, _) =
  match k with "dsrc" | "escache" | "templ" -> true | _ -> false

let key_env conf base =
  let scratch s = Mutil.encode (Name.lower (Driver.sou base s)) in
  let get_a_person v =
    try
      let i = Driver.Iper.of_string (Mutil.decode v) in
      let p = Util.pget conf base i in
      if
        (Util.is_hide_names conf p && not (Util.authorized_age conf base p))
        || Util.is_hidden p
      then None
      else
        let f = scratch (Driver.get_first_name p) in
        let s = scratch (Driver.get_surname p) in
        let oc = string_of_int (Driver.get_occ p) |> Adef.encoded in
        Some (f, s, oc)
    with Failure _ -> None
  in
  let get_a_family v =
    try
      let i = Driver.Ifam.of_string (Mutil.decode v) in
      let fam = Driver.foi base i in
      let p = Util.pget conf base (Driver.get_father fam) in
      let f = scratch (Driver.get_first_name p) in
      let s = scratch (Driver.get_surname p) in
      if
        (f : Adef.encoded_string :> string) = ""
        || (s : Adef.encoded_string :> string) = ""
      then None
      else
        let oc = string_of_int (Driver.get_occ p) |> Adef.encoded in
        let fams = Driver.get_family p in
        let rec loop k =
          if k >= Array.length fams then None
          else if fams.(k) = i then Some (string_of_int k |> Adef.encoded)
          else loop (k + 1)
        in
        match loop 0 with None -> None | Some n -> Some (f, s, oc, n)
    with Failure _ -> None
  in
  let rec loop :
      (string * Adef.encoded_string) list -> (string * Adef.encoded_string) list
      = function
    | [] -> []
    | kv :: l when drop kv -> loop l
    | ("i", v) :: l -> new_env "i" v (fun x -> x) l
    | ("ei", v) :: l -> new_env "ei" v (fun x -> "e" ^ x) l
    | (k, v) :: l when String.length k = 2 && k.[0] = 'i' ->
        let c = String.make 1 k.[1] in
        new_env k v (fun x -> x ^ c) l
    | (k, (v : Adef.encoded_string)) :: l
      when String.length k > 2 && k.[0] = 'e' && k.[1] = 'f' ->
        new_fam_env k v (fun x -> x ^ k) l
    | kv :: l -> kv :: loop l
  and new_env k (v : Adef.encoded_string) c l :
      (string * Adef.encoded_string) list =
    match get_a_person v with
    | Some (f, s, oc) ->
        if (oc :> string) = "0" then (c "p", f) :: (c "n", s) :: loop l
        else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: loop l
    | None -> (k, v) :: loop l
  and new_fam_env k (v : Adef.encoded_string) c l =
    match get_a_family v with
    | Some (f, s, oc, n) ->
        let l = loop l in
        let l = if (n :> string) = "0" then l else (c "f", n) :: l in
        if (oc :> string) = "0" then (c "p", f) :: (c "n", s) :: l
        else (c "p", f) :: (c "n", s) :: (c "oc", oc) :: l
    | None -> (k, v) :: loop l
  in
  loop conf.env

let query_of_env conf env : Adef.encoded_string =
  List.fold_right
    (fun (x, v) s ->
      if (v : Adef.encoded_string :> string) <> "" then
        let amp =
          if (s : Adef.encoded_string :> string) = "" then "" else "&"
        in
        x ^ "=" ^ (v :> string) ^ amp ^ (s :> string) |> Adef.encoded
      else s)
    (("lang", Mutil.encode conf.lang) :: env)
    (Adef.encoded "")

let query conf base = query_of_env conf (key_env conf base)

let query_aux conf =
  query_of_env conf (List.filter (fun kv -> not (drop kv)) conf.env)

let script conf (q : Adef.encoded_string) : Adef.safe_string =
  let esc s =
    let b = Buffer.create (String.length s + 8) in
    String.iter
      (function
        | '"' -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | '<' -> Buffer.add_string b "\\u003c"
        | '/' -> Buffer.add_string b "\\/"
        | c -> Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  let trn s n = esc (Util.transl_nth conf s n) in
  let cap = esc (Utf8.capitalize_fst (Util.transl conf "copy permalink")) in
  let role = if conf.wizard then 2 else if conf.friend then 1 else 0 in
  let vr = "wizard/wizards/friend/friends/exterior" in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf {|{"q":"%s"|} (esc (q :> string)));
  if role > 0 then Buffer.add_string buf (Printf.sprintf {|,"r":%d|} role);
  Buffer.add_string buf
    (Printf.sprintf {|,"t":{"c":"%s","p":"%s"|} cap (trn vr 4));
  if role >= 1 then
    Buffer.add_string buf (Printf.sprintf {|,"f":"%s"|} (trn vr 2));
  if role >= 2 then
    Buffer.add_string buf (Printf.sprintf {|,"w":"%s"|} (trn vr 0));
  Buffer.add_string buf "}}";
  let json = Buffer.contents buf in
  let pfx =
    let p = if conf.cgi then conf.etc_prefix else "" in
    if p = "" then "" else p ^ "/"
  in
  let file = pfx ^ "js/permalink.min.js" in
  let fpath = if conf.cgi then file else Util.resolve_asset_file conf file in
  let src =
    match Util.hash_file_cached fpath with
    | Some h -> file ^ "?hash=" ^ h
    | None -> file
  in
  Adef.safe
    (Printf.sprintf
       "<script>window.GWPERMA=%s;</script>\n<script defer src=\"%s\"></script>"
       json src)

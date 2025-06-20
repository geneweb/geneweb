open Geneweb
open Config
open Adef
module Driver = Geneweb_db.Driver

let ns = "no_index"

let url_no_index conf base pwd =
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
        let u = Util.pget conf base (Driver.get_father fam) in
        let n =
          let rec loop k =
            if (Driver.get_family u).(k) = i then
              string_of_int k |> Adef.encoded
            else loop (k + 1)
          in
          loop 0
        in
        Some (f, s, oc, n)
    with Failure _ -> None
  in
  let env =
    let rec loop :
        (string * Adef.encoded_string) list ->
        (string * Adef.encoded_string) list = function
      | [] -> []
      | ("opt", s) :: l when (s :> string) = "no_index" -> loop l
      | ("opt", s) :: l when (s :> string) = "no_index_pwd" -> loop l
      | (("dsrc" | "escache" | "templ"), _) :: l -> loop l
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
  let suff : Adef.encoded_string =
    List.fold_right
      (fun (x, v) s ->
        if (v : Adef.encoded_string :> string) <> "" then
          x ^<^ "=" ^<^ v
          ^^^ (if (s : Adef.encoded_string :> string) = "" then "" else "&")
          ^<^ s
        else s)
      (("lang", Mutil.encode conf.lang) :: env)
      (Adef.encoded "")
  in
  if conf.cgi then addr ^<^ "?b=" ^<^ conf.bname ^<^ "&" ^<^ suff
  else addr ^<^ "?" ^<^ suff

let w_base =
  let none conf = Hutil.incorrect_request conf in
  Gwd_lib.Request.w_base ~none

let no_index conf base =
  let opt1 = Util.p_getenv conf.env "opt" = Some "no_index" in
  let opt2 = Util.p_getenv conf.env "opt" = Some "no_index_pwd" in
  if opt1 || opt2 then (
    let link = url_no_index conf base opt2 in
    Output.print_sstring conf {|<a href="http://|};
    Output.print_string conf link;
    Output.print_sstring conf {|">|};
    Output.print_string conf link;
    Output.print_sstring conf "</a>";
    Output.flush conf;
    exit 0)

let () = Gwd_lib.GwdPlugin.register_se ~ns @@ fun _assets -> w_base no_index

open Config
open Dag2html
open Def
open Util
open Dag

let src = Logs.Src.create ~doc:"DagDisplay" __MODULE__

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver

let image_normal_txt conf base p fname width height =
  let image_txt = Utf8.capitalize_fst (transl_nth conf "image/images" 0) in
  let s = Unix.stat fname in
  let k = Image.default_image_filename "portraits" base p in
  let r =
    Format.sprintf
      {|<img src="%sm=IM&d=%s&%s&k=%s"%s%s alt="%s" title="%s"
        style="%s %s">|}
      (commd conf : Adef.escaped_string :> string)
      (string_of_int
      @@ int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
      (acces conf base p : Adef.escaped_string :> string)
      k
      (if width = 0 then "" else " width=" ^ string_of_int width)
      (if height = 0 then "" else " height=" ^ string_of_int height)
      image_txt image_txt
      (if width = 0 then "" else "max-width:" ^ string_of_int width ^ "px;")
      (if height = 0 then "" else "max-height:" ^ string_of_int height ^ "px;")
  in
  (if p_getenv conf.env "cgl" = Some "on" then r
   else
     Format.sprintf {|<a href="%sm=IM&%s&k=%s">%s</a>|}
       (commd conf : Adef.escaped_string :> string)
       (acces conf base p : Adef.escaped_string :> string)
       k r)
  |> Adef.safe

let image_url_txt conf url_p url ~width ~height =
  let width = Option.value ~default:0 width in
  let image_txt = Utf8.capitalize_fst (transl_nth conf "image/images" 0) in
  let size =
    Format.sprintf "%s %s"
      (if width = 0 then "" else Format.sprintf {|width="%d"|} width)
      (if height = 0 then "" else Format.sprintf {|height="%d"|} height)
  in
  let style =
    Format.sprintf "%spx;%spx;"
      (if width = 0 then "" else "max-width:" ^ string_of_int width)
      (if height = 0 then "" else "max-height:" ^ string_of_int height)
  in
  let s =
    Format.sprintf {|<img src="%s" alt="%s" title="%s" %s style="%s">|}
      (url : Adef.escaped_string :> string)
      image_txt image_txt size style
  in
  Adef.safe
  @@
  if p_getenv conf.env "cgl" = Some "on" then s
  else
    Format.sprintf {|<a href="%s">%s</a>|}
      (url_p : Adef.escaped_string :> string)
      s

let image_txt conf base p =
  let img = Util.get_opt conf "im" true in
  Adef.safe
  @@
  if img then
    match Image.get_portrait_with_size conf base p with
    | None -> ""
    | Some (`Path s, size_opt) ->
        let max_w, max_h = (100, 75) in
        let w, h =
          match size_opt with
          | Some (w, h) -> Image.scale_to_fit ~max_w ~max_h ~w ~h
          | None -> (0, max_h)
        in
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (image_normal_txt conf base p s w h |> Adef.as_string)
    | Some (`Url url, Some (width, height)) ->
        let url_p = commd conf ^^^ acces conf base p in
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (image_url_txt conf url_p (Util.escape_html url) ~width:(Some width)
             ~height
          |> Adef.as_string)
    | Some (`Url url, None) ->
        let url_p = commd conf ^^^ acces conf base p in
        let height = 75 in
        (* La hauteur est ajoutée à la table pour que les textes soient alignés. *)
        Printf.sprintf
          {|
            <br>
            <center>
              <table border="0" style="height:%spx">
                <tr align="left"><td>%s</td></tr>
              </table>
            </center>|}
          (string_of_int height)
          (image_url_txt conf url_p (Util.escape_html url) ~width:None ~height
          |> Adef.as_string)
  else ""

type item = Item of Driver.person * Adef.safe_string

let string_of_item conf base = function
  | Item (p, s) ->
      Util.referenced_person_title_text conf base p
      ^^^ DateDisplay.short_dates_text conf base p
      ^^^ if (s :> string) = "" then Adef.safe "" else " " ^<^ s

let make_tree_hts conf base elem_txt vbar_txt invert set spl d =
  let set_lookup =
    List.fold_left (Fun.flip Dag.Iperset.add) Dag.Iperset.empty set
  in
  let no_group = p_getenv conf.env "nogroup" = Some "on" in
  let spouse_on =
    match (Util.p_getenv conf.env "sp", Util.p_getenv conf.env "spouse") with
    | Some ("off" | "0"), _ | _, Some "off" -> false
    | _, _ -> true
  in
  let bd = match Util.p_getint conf.env "bd" with Some x -> x | None -> 0 in
  let indi_ip n =
    match n.valu with Left ip -> ip | Right _ -> Driver.Iper.dummy
  in
  let indi_txt n =
    match n.valu with
    | Left ip ->
        let p = pget conf base ip in
        let txt =
          image_txt conf base p ^^^ string_of_item conf base (elem_txt p)
        in
        let spouses =
          if ((spouse_on && n.chil <> []) || n.pare = []) && not invert then
            List.fold_left
              (fun list id ->
                match d.dag.(int_of_idag id).valu with
                | Left cip -> (
                    match Driver.get_parents (pget conf base cip) with
                    | Some ifam ->
                        let cpl = Driver.foi base ifam in
                        if ip = Driver.get_father cpl then
                          if List.mem_assoc (Driver.get_mother cpl) list then
                            list
                          else (Driver.get_mother cpl, Some ifam) :: list
                        else if ip = Driver.get_mother cpl then
                          if List.mem_assoc (Driver.get_father cpl) list then
                            list
                          else (Driver.get_father cpl, Some ifam) :: list
                        else list
                    | None -> list)
                | Right _ -> list)
              [] n.chil
          else if n.chil = [] then
            try [ List.assq ip spl ] with Not_found -> []
          else []
        in
        List.fold_left
          (fun txt (ips, ifamo) ->
            if Dag.Iperset.mem ips set_lookup then txt
            else
              let ps = pget conf base ips in
              let auth =
                authorized_age conf base p && authorized_age conf base ps
              in
              let d =
                match ifamo with
                | Some ifam when auth ->
                    DateDisplay.short_marriage_date_text conf base
                      (Driver.foi base ifam) p ps
                | _ -> Adef.safe ""
              in
              txt ^^^ "<br>&amp;" ^<^ d ^^^ " "
              ^<^ string_of_item conf base (elem_txt ps)
              ^^^ image_txt conf base ps)
          txt spouses
    | Right _ -> Adef.safe "&nbsp;"
  in
  let indi_txt n : Adef.safe_string =
    let bd = match n.valu with Left _ -> bd | _ -> 0 in
    if bd > 0 then
      {|<table border="|} ^<^ string_of_int bd
      ^<^ {|"><tr align="left"><td align="center">|} ^<^ indi_txt n
      ^>^ {|</td></tr></table>|}
    else indi_txt n
  in
  let vbar_txt n =
    match n.valu with Left ip -> vbar_txt ip | _ -> Adef.escaped ""
  in
  let phony n = match n.valu with Left _ -> false | Right _ -> true in
  let t = Dag2html.table_of_dag phony false invert no_group d in
  if Array.length t.table = 0 then [||]
  else Dag2html.html_table_struct indi_ip indi_txt vbar_txt phony d t

type 'a env =
  | Vbool of bool
  | Vcnt of int ref
  | Vdcell of (int * Dag2html.align * Adef.safe_string Dag2html.table_data)
  | Vdline of int
  | Vint of int
  | Vsstring of Adef.safe_string
  | Vestring of Adef.escaped_string
  | Vind of Driver.person
  | Vother of 'a
  | Vnone
  | Vvars of (string * string) list ref

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

(* TODO should vl be Vint list and f = max|min, not a string?*)
let eval_predefined_apply f vl =
  let vl =
    List.map
      (function
        | Templ.VVstring "" -> (
            match f with
            | "min" -> max_int
            | "max" -> 0 - max_int
            | _ -> raise Not_found)
        | VVstring s -> int_of_string s
        | _ -> raise Not_found)
      vl
  in
  let f, first_element, l =
    match (f, vl) with
    | "min", s :: sl -> (min, s, sl)
    | "max", s :: sl -> (max, s, sl)
    | _ -> raise Not_found
  in
  try
    let m = List.fold_left (fun acc s -> f acc s) first_element l in
    string_of_int m
  with Failure _ ->
    Log.warn (fun k -> k "Incorrect parameter for eval_predefined_apply");
    raise Not_found

let parents_access_aux conf base td get_parent =
  match td with
  | TDitem (ip, _, _) | TDtext (ip, _) -> (
      match Driver.get_parents (Driver.poi base ip) with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          Templ.VVstring
            (Util.acces conf base (Driver.poi base (get_parent cpl)) :> string)
      | None -> VVstring "")
  | _ -> VVstring ""

let has_sibling_aux base td next_or_prev =
  match td with
  | TDitem (ip, _, _) | TDtext (ip, _) -> (
      match Driver.get_parents (Driver.poi base ip) with
      | Some ifam ->
          let sib = Driver.get_children (Driver.foi base ifam) in
          (* array *)
          let i = ref (-1) in
          let _ = Array.iteri (fun n s -> if ip = s then i := n else ()) sib in
          let cond =
            if next_or_prev then !i >= 0 && !i < Array.length sib - 1
            else !i >= 1
          in
          if cond then Some sib.(!i + if next_or_prev then 1 else -1) else None
      | None -> None)
  | _ -> None

let sibling_access_aux conf base td next_or_prev =
  match has_sibling_aux base td next_or_prev with
  | Some s_ip ->
      Templ.VVstring (Util.acces conf base (Driver.poi base s_ip) :> string)
  | None -> raise Not_found

let rec eval_var conf base env _xx _loc = function
  | [ "browsing_with_sosa_ref" ] -> (
      match (Util.p_getenv conf.env "pz", Util.p_getenv conf.env "nz") with
      | Some _, Some _ -> Templ.VVbool true
      | _, _ -> VVbool false)
  | [ "cell_nbr" ] -> (
      match get_env "cell_nbr" env with
      | Vint i -> Templ.VVstring (string_of_int i)
      | _ -> raise Not_found)
  | "dag_cell" :: sl -> (
      match get_env "dag_cell" env with
      | Vdcell dcell -> eval_dag_cell_var conf base env dcell sl
      | _ -> raise Not_found)
  | [ "head_title" ] -> (
      match get_env "p_title" env with
      | Vsstring s -> VVstring (s :> string)
      | _ -> VVstring "")
  | [ "is_first" ] -> (
      match get_env "first" env with Vbool b -> VVbool b | _ -> VVbool false)
  | [ "is_last" ] -> (
      match get_env "last" env with Vbool b -> VVbool b | _ -> VVbool false)
  | [ "line_nbr" ] -> (
      match get_env "line_nbr" env with
      | Vint i -> VVstring (string_of_int i)
      | _ -> raise Not_found)
  | [ "link_next" ] -> (
      match get_env "next_txt" env with
      | Vestring s -> VVstring (s :> string)
      | _ -> VVstring "")
  | [ "person_index" ] -> (
      match find_person_in_env conf base "" with
      | Some p -> VVstring (Driver.Iper.to_string (Driver.get_iper p))
      | None -> VVstring "")
  (* person_index.x -> i=, p=, n=, oc= *)
  (* person_index.1 -> i1=, p1=, n1=, oc1= *)
  (* person_index.2 -> i2=, p2=, n2=, oc2= *)
  (* person_index.e -> ei=, ep=, en=, eoc= *)
  (* same thing in perso.ml, but differences! *)
  | [ "person_index"; x; sl ] -> (
      let find_person =
        match x with "e" -> find_person_in_env_pref | _ -> find_person_in_env
      in
      let s = if x = "x" then "" else x in
      match find_person conf base s with
      | Some p ->
          let auth = authorized_age conf base p in
          let ep = (p, auth) in
          eval_person_field_var conf base env ep sl
      | None -> (
          match p_getenv conf.env s with
          | Some s when Option.is_some (int_of_string_opt s) ->
              let p = Driver.poi base (Driver.Iper.of_string s) in
              let auth = authorized_age conf base p in
              let ep = (p, auth) in
              eval_person_field_var conf base env ep sl
          | _ -> VVstring ""))
  | [ "person_index"; x ] -> (
      let find_person =
        match x with "e" -> find_person_in_env_pref | _ -> find_person_in_env
      in
      let s = if x = "x" then "" else x in
      match find_person conf base s with
      | Some p -> VVstring (Driver.Iper.to_string (Driver.get_iper p))
      | None -> VVstring "")
  | [ "get_var"; name ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          let vv =
            try List.assoc name !lv with Not_found -> raise Not_found
          in
          VVstring vv
      | _ -> raise Not_found)
  | [ "set_var"; name; value ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          if List.mem_assoc name !lv then lv := List.remove_assoc name !lv;
          lv := (name, value) :: !lv;
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          VVstring ""
      | _ -> raise Not_found)
  (* TODO set real values *)
  | [ "static_max_anc_level" ] -> VVstring "10"
  | [ "static_max_desc_level" ] -> VVstring "10"
  | _ -> raise Not_found

and eval_person_field_var _conf base _env (p, pauth) = function
  | "surname" when pauth -> VVstring (Driver.p_surname base p)
  | "first_name" when pauth ->
      VVstring (Driver.p_first_name base p)
  | _ -> raise Not_found

and eval_dag_cell_var conf base env (colspan, align, td) = function
  | [ "access" ] -> (
      match td with
      | TDtext (ip, _s) ->
          VVstring (Util.acces conf base (Driver.poi base ip) :> string)
      | _ -> VVstring "")
  | [ "align" ] -> (
      match align with
      | LeftA -> VVstring conf.left
      | CenterA -> VVstring "center"
      | RightA -> VVstring conf.right)
  | [ "bar_link" ] ->
      VVstring
        (match td with
        | TDbar (Some s) -> (s : Adef.escaped_string :> string)
        | _ -> "")
  | [ "colspan" ] -> VVstring (string_of_int colspan)
  | [ "father"; "access" ] -> parents_access_aux conf base td Driver.get_father
  | [ "has_next_sibling" ] -> (
      match has_sibling_aux base td true with
      | Some _ -> VVbool true
      | None -> VVbool false)
  | [ "has_prev_sibling" ] -> (
      match has_sibling_aux base td false with
      | Some _ -> VVbool true
      | None -> VVbool false)
  | [ "index" ] -> (
      match td with
      | TDitem (ip, _, _) | TDtext (ip, _) ->
          VVstring (Driver.Iper.to_string ip)
      | _ -> VVstring "")
  | [ "is_bar" ] -> VVbool (match td with TDbar _ -> true | _ -> false)
  | [ "is_hr" ] -> (
      match td with
      | TDhr RightA | TDhr LeftA | TDhr CenterA -> VVbool true
      | _ -> VVbool false)
  | [ "is_hr_center" ] -> (
      match td with TDhr CenterA -> VVbool true | _ -> VVbool false)
  | [ "is_hr_left" ] -> (
      match td with TDhr LeftA -> VVbool true | _ -> VVbool false)
  | [ "is_hr_right" ] -> (
      match td with TDhr RightA -> VVbool true | _ -> VVbool false)
  | [ "is_nothing" ] -> VVbool (td = TDnothing)
  | [ "item" ] -> (
      match td with
      | TDitem (_ip, s, _t) -> VVstring (s : Adef.safe_string :> string)
      | _ -> VVstring "")
  | [ "mother"; "access" ] -> parents_access_aux conf base td Driver.get_mother
  | [ "next_sibling"; "access" ] -> sibling_access_aux conf base td true
  | [ "prev_sibling"; "access" ] -> sibling_access_aux conf base td false
  | [ "text" ] -> (
      match td with
      | TDtext (_ip, s) -> VVstring (s : Adef.safe_string :> string)
      | _ -> VVstring "")
  | [ "get_var"; name ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          let vv =
            try List.assoc name !lv with Not_found -> raise Not_found
          in
          VVstring vv
      | _ -> VVstring "")
  | [ "set_var"; name; value ] -> (
      match get_env "vars" env with
      | Vvars lv ->
          if List.mem_assoc name !lv then lv := List.remove_assoc name !lv;
          lv := (name, value) :: !lv;
          (if not (List.mem name !GWPARAM.set_vars) then
             let name =
               if name.[0] = ' ' then String.sub name 1 (String.length name - 1)
               else name
             in
             GWPARAM.set_vars := name :: !GWPARAM.set_vars);
          VVstring ""
      | _ -> raise Not_found)
  | _ -> raise Not_found

let rec print_foreach _conf hts print_ast _eval_expr env () _loc s sl _el al =
  match s :: sl with
  | [ "dag_cell" ] -> print_foreach_dag_cell hts print_ast env al
  | [ "dag_line" ] -> print_foreach_dag_line print_ast env hts al
  | _ -> raise Not_found

and print_foreach_dag_cell hts print_ast env al =
  match get_env "dag_line" env with
  | Vdline i ->
      for j = 0 to Array.length hts.(i) - 1 do
        let env =
          Templ.Env.(
            env
            |> add "dag_cell" (Vdcell hts.(i).(j))
            |> add "cell_nbr" (Vint j)
            |> add "first" (Vbool (j = 0))
            |> add "last" (Vbool (j = Array.length hts.(i) - 1)))
        in
        List.iter (print_ast env ()) al
      done
  | _ -> raise Not_found

and print_foreach_dag_line print_ast env hts al =
  for i = 0 to Array.length hts - 1 do
    let env =
      Templ.Env.(
        env |> add "dag_line" (Vdline i) |> add "line_nbr" (Vint i)
        |> add "first" (Vbool (i = 0))
        |> add "last" (Vbool (i = Array.length hts - 1)))
    in
    List.iter (print_ast env ()) al
  done

let print_dag_page conf base page_title hts next_txt =
  let p =
    match find_person_in_env conf base "" with
    | Some p -> p
    | None -> Driver.poi base Driver.Iper.dummy
  in
  let env =
    Templ.Env.empty |> Templ.Env.add "p" (Vind p)
    |> Templ.Env.add "p_auth" (Vbool (authorized_age conf base p))
    |> Templ.Env.add "count" (Vcnt (ref 0))
    |> Templ.Env.add "count1" (Vcnt (ref 0))
    |> Templ.Env.add "count2" (Vcnt (ref 0))
    |> Templ.Env.add "count3" (Vcnt (ref 0))
    |> Templ.Env.add "vars" (Vvars (ref []))
    |> Templ.Env.add "p_title" (Vsstring page_title)
    |> Templ.Env.add "next_txt" (Vestring next_txt)
  in
  let ifun =
    Templ.
      {
        eval_var = eval_var conf base;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> eval_predefined_apply);
        get_vother;
        set_vother;
        print_foreach = print_foreach conf hts;
      }
  in
  Templ.output conf ifun env () "dag"

let make_and_print_dag conf base elem_txt vbar_txt invert set spl page_title
    next_txt =
  let d = make_dag conf base set in
  let hts = make_tree_hts conf base elem_txt vbar_txt invert set spl d in
  print_dag_page conf base page_title hts next_txt

let print conf base =
  let has_pnoc_params = Util.url_has_pnoc_params conf.env in
  let has_s1 = Util.p_getenv conf.env "s1" <> None in
  if has_pnoc_params && not has_s1 then (
    let all_indexes =
      List.fold_left
        (fun acc (key, _) ->
          if String.length key >= 2 then
            let prefix = String.get key 0 in
            let idx_str = String.sub key 1 (String.length key - 1) in
            try
              let idx = int_of_string idx_str in
              if (prefix = 'p' || prefix = 's') && not (List.mem idx acc) then
                idx :: acc
              else acc
            with _ -> acc
          else acc)
        [] conf.env
      |> List.sort compare
    in
    let converted_params = ref [] in
    List.iter
      (fun idx ->
        let k = string_of_int idx in
        (* Traiter p* *)
        (match p_getenv conf.env ("p" ^ k) with
        | Some fn when fn <> "" -> (
            let sn = Option.value ~default:"" (p_getenv conf.env ("n" ^ k)) in
            let oc =
              try
                int_of_string
                  (Option.value ~default:"0" (p_getenv conf.env ("oc" ^ k)))
              with _ -> 0
            in
            match Driver.person_of_key base fn sn oc with
            | Some ip ->
                converted_params :=
                  ("i" ^ k ^ "=" ^ Driver.Iper.to_string ip)
                  :: !converted_params
            | None -> ())
        | _ -> ());
        match p_getenv conf.env ("s" ^ k) with
        | Some s when s <> "" ->
            converted_params :=
              ("s" ^ k ^ "=" ^ (Mutil.encode s :> string)) :: !converted_params
        | _ -> ())
      all_indexes;
    let clean_url =
      Printf.sprintf "%s?m=DAG&%s"
        (Util.prefix_base conf :> string)
        (String.concat "&" (List.rev !converted_params))
    in
    Wserver.http_redirect_temporarily clean_url)
  else
    let set = get_dag_elems conf base in
    let elem_txt p = Item (p, Adef.safe "") in
    let vbar_txt _ = Adef.escaped "" in
    let invert = Util.p_getenv conf.env "invert" = Some "on" in
    let page_title =
      Util.transl conf "tree" |> Utf8.capitalize_fst |> Adef.safe
    in
    make_and_print_dag conf base elem_txt vbar_txt invert set [] page_title
      (Adef.escaped "")

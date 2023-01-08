open Config
open Gwdb
open TemplAst
open Util
open UpdateData

let translate_title conf =
  let title =
    match p_getenv conf.env "data" with
    | Some "occu" -> transl_nth conf "occupation/occupations" 1
    | Some "place" -> transl conf "places"
    | Some "src" -> transl_nth conf "source/sources" 1
    | Some "fn" -> transl_nth conf "first name/first names" 1
    | Some "sn" -> transl_nth conf "surname/surnames" 1
    | _ -> ""
  in
  (Printf.sprintf (ftransl conf "book of %s") title, title)

(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)

(* ******************************************************************** *)

(** [Description] : Met à jour toutes les personnes en relation avec
                    la donnée que l'on veut modifié.
    [Args] :
    - conf : configuration
    - base : base
      [Retour] :
    - unit
      [Rem] : Non exporté en clair hors de ce module.                     *)
let print_mod_ok conf base =
  let data = Option.value ~default:"" (p_getenv conf.env "data") in
  let ini = Option.value ~default:"" (p_getenv conf.env "s") in
  let new_input =
    Option.fold ~none:"" ~some:only_printable (p_getenv conf.env "nx_input")
  in
  let list = get_person_from_data conf base in
  let list = List.map (fun (istr, perl) -> (sou base istr, perl)) list in
  let nb_pers =
    List.fold_left (fun accu (_, perl) -> accu + List.length perl) 0 list
  in
  let data_modified = List.for_all (fun (old, _) -> new_input <> old) list in
  (* Indication : 1000 fiches prend environ 1 seconde de traitement. *)
  (* Attention à ne pas mettre une limite trop grande (d'où le test) *)
  (* pour ne pas dépasser le time out du serveur.                    *)
  let max_updates =
    match List.assoc_opt "max_nb_update" conf.base_env with
    | Some n ->
        let n = int_of_string n in
        if n > 50000 then 5000 else n
    | _ -> 5000
  in
  if nb_pers <> 0 && data_modified then (
    update_person_list conf base new_input list nb_pers max_updates;
    let title _ =
      transl conf "modification successful"
      |> Utf8.capitalize_fst |> Output.print_sstring conf
    in
    Hutil.header conf title;
    Hutil.print_link_to_welcome conf true;
    Output.print_sstring conf "<p>";
    transl conf "modification successful"
    |> Utf8.capitalize_fst |> Output.print_sstring conf;
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf " ";
    Output.print_sstring conf (min nb_pers max_updates |> string_of_int);
    if List.assoc_opt "history" conf.base_env = Some "yes" then (
      Output.print_sstring conf "<a href=\"";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=HIST&k=20\">";
      Output.print_sstring conf
        (transl_nth conf "modification/modifications"
           (if nb_pers > 1 then 1 else 0));
      Output.print_sstring conf ".</a>")
    else (
      Output.print_sstring conf
        (transl_nth conf "modification/modifications"
           (if nb_pers > 1 then 1 else 0));
      Output.print_sstring conf ".");
    Output.print_sstring conf "</p>";
    if nb_pers > max_updates then (
      Output.printf conf {|<form method="post" action="%s"><p>|} conf.command;
      Util.hidden_env conf;
      Util.hidden_input conf "key" (List.assoc "key" conf.env);
      Util.hidden_input conf "m" (Adef.encoded "MOD_DATA_OK");
      Util.hidden_input conf "data" (Mutil.encode data);
      Util.hidden_input conf "s" (Mutil.encode ini);
      Output.print_sstring conf
        {|<input type="hidden" name="nx_input" size="80" maxlength="200" value="|};
      Output.print_string conf (Util.escape_html (only_printable new_input));
      Output.print_sstring conf {|" id="data">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "continue correcting"));
      Output.print_sstring conf
        {|<button type="submit" class="btn btn-secondary btn-lg">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
      Output.print_sstring conf "</button></p></form>");
    Output.print_sstring conf {|<p><a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=MOD_DATA&data=|};
    Output.print_string conf (Mutil.encode data);
    Output.print_sstring conf {|&s=|};
    Output.print_string conf (Mutil.encode ini);
    Output.print_sstring conf {|" id="reference">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "new modification"));
    Output.print_sstring conf {|</a></p>|};
    Hutil.trailer conf)
  else (
    Hutil.header conf (fun _ ->
        transl conf "no modification"
        |> Utf8.capitalize_fst |> Output.print_sstring conf);
    Hutil.print_link_to_welcome conf true;
    Output.print_sstring conf {|<p><a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=MOD_DATA&data=|};
    Output.print_string conf (Mutil.encode data);
    Output.print_sstring conf {|&s=|};
    Output.print_string conf (Mutil.encode ini);
    Output.print_sstring conf {|" id="reference">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "new modification"));
    Output.print_sstring conf {|</a></p>|};
    Hutil.trailer conf)

type 'a env =
  | Vbool of bool
  | Vcnt of int ref
  | Vint of int
  | Vlist_data of (istr * string) list
  | Vlist_value of (istr * string) list
  | Vnone
  | Vother of 'a
  | Vstring of string

let string_to_list str =
  let rec loop acc = function
    | s ->
        if String.length s > 0 then
          let nbc = Utf8.nbc s.[0] in
          let c = String.sub s 0 nbc in
          let s1 = String.sub s nbc (String.length s - nbc) in
          loop (c :: acc) s1
        else acc
  in
  loop [] str

let unfold_place_long inverted s =
  let pl = Place.fold_place_long inverted s in
  String.concat ", " pl

let get_env v env = try List.assoc v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x
let bool_val x = VVbool x
let str_val x = VVstring x

let rec eval_var conf base env xx _loc sl =
  try eval_simple_var conf base env xx sl
  with Not_found -> eval_compound_var conf base env xx sl

and eval_simple_var conf base env xx = function
  | [ s ] -> (
      try bool_val (eval_simple_bool_var conf base env xx s)
      with Not_found -> str_val (eval_simple_str_var conf base env xx s))
  | _ -> raise Not_found

and eval_simple_bool_var _conf _base env _xx = function
  | "is_modified" -> (
      match get_env "key" env with
      | Vstring _ as x -> get_env "entry_key" env = x
      | _ -> false)
  | "first" -> ( match get_env "first" env with Vbool x -> x | _ -> false)
  | _ -> raise Not_found

and eval_simple_str_var conf _base env _xx = function
  | "cnt" -> eval_int_env "cnt" env
  | "count" -> (
      match get_env "count" env with Vcnt c -> string_of_int !c | _ -> "")
  | "entry_ini" -> eval_string_env "entry_ini" env
  | "entry_value" -> eval_string_env "entry_value" env
  | "entry_value_rev" -> eval_string_env "entry_value_rev" env
  | "entry_key" -> eval_string_env "entry_key" env
  | "ini" -> eval_string_env "ini" env
  | "incr_count" -> (
      match get_env "count" env with
      | Vcnt c ->
          incr c;
          ""
      | _ -> "")
  | "max" -> eval_int_env "max" env
  | "nb_results" -> (
      match get_env "list" env with
      | Vlist_data l -> string_of_int (List.length l)
      | _ -> "0")
  | "other" -> (
      match p_getenv conf.env "data" with
      | Some "place" -> Place.without_suburb (eval_string_env "entry_value" env)
      | _ -> "")
  | "reset_count" -> (
      match get_env "count" env with
      | Vcnt c ->
          c := 0;
          ""
      | _ -> "")
  | "suburb" -> (
      match p_getenv conf.env "data" with
      | Some "place" -> Place.only_suburb (eval_string_env "entry_value" env)
      | _ -> "")
  | "substr" -> eval_string_env "substr" env
  | "tail" -> eval_string_env "tail" env
  | "title" ->
      let len =
        match get_env "list" env with Vlist_data l -> List.length l | _ -> 0
      in
      let ini = Option.value ~default:"" (p_getenv conf.env "s") in
      let book_of, title = translate_title conf in
      let result =
        if ini = "" then Printf.sprintf " (%d %s)" len title
        else
          let ini = Adef.as_string @@ Mutil.encode ini in
          " - "
          ^ Printf.sprintf (ftransl conf "%d %s starting with %s") len title ini
      in
      Utf8.capitalize_fst book_of ^ result
  | _ -> raise Not_found

and eval_compound_var conf base env xx sl =
  let rec loop = function
    | [ s ] -> eval_simple_str_var conf base env xx s
    | [ "evar"; "length"; s ] | [ "e"; "length"; s ] -> (
        match p_getenv conf.env s with
        | Some s -> string_of_int (String.length s)
        | None -> "")
    | [ "evar"; "p"; s ] | [ "e"; "p"; s ] -> (
        match p_getenv conf.env s with
        | Some s ->
            if String.length s > 1 then String.sub s 0 (String.length s - 1)
            else ""
        | None -> "")
    | [ "evar"; s ] | [ "e"; s ] ->
        Option.value ~default:"" (p_getenv conf.env s)
    | "encode" :: sl -> (Mutil.encode (loop sl) :> string) (* FIXME? *)
    | ("escape" | "html_encode") :: sl ->
        (Util.escape_html (loop sl) :> string) (* FIXME? *)
    | "safe" :: sl -> (Util.safe_html (loop sl) :> string) (* FIXME? *)
    | [ "subs"; n; s ] -> (
        match int_of_string_opt n with
        | Some n ->
            if String.length s > n then String.sub s 0 (String.length s - n)
            else (
              Printf.sprintf "String shorter that requested\n"
              |> !GWPARAM.syslog `LOG_WARNING;
              s)
        | None -> raise Not_found)
    | "printable" :: sl -> only_printable (loop sl)
    | _ -> raise Not_found
  in
  str_val (loop sl)

and eval_string_env s env =
  match get_env s env with Vstring s -> s | _ -> raise Not_found

and eval_int_env s env =
  match get_env s env with Vint i -> string_of_int i | _ -> raise Not_found

let print_foreach conf print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl el al =
    match s :: sl with
    | [ "initial" ] -> print_foreach_initial env xx al
    | [ "entry" ] -> print_foreach_entry env xx el al
    | [ "substr"; e ] -> print_foreach_substr env xx el al e
    | [ "value" ] -> print_foreach_value env xx al
    | _ -> raise Not_found
  and print_foreach_entry env xx _el al =
    let list = match get_env "list" env with Vlist_data l -> l | _ -> [] in
    let list = build_list_long conf list in
    let max = List.length list in
    let k = Option.value ~default:"" (p_getenv conf.env "key") in
    List.iteri
      (fun i (ini_k, (list_v : (istr * string) list)) ->
        let env =
          ("cnt", Vint i) :: ("max", Vint max) :: ("key", Vstring k)
          :: ("entry_ini", Vstring ini_k)
          :: ("list_value", Vlist_value list_v)
          :: env
        in
        List.iter (print_ast env xx) al)
      list
  and print_foreach_substr env xx _el al evar =
    let evar = match p_getenv conf.env evar with Some s -> s | None -> "" in
    let list_of_char = string_to_list evar in
    let list_of_sub =
      let rec loop acc = function
        | c :: l ->
            let s1 = List.fold_left (fun acc cc -> cc ^ acc) c l in
            loop (s1 :: acc) l
        | [] -> acc
      in
      loop [] list_of_char
    in
    let max = List.length list_of_sub in
    let rec loop first cnt = function
      | s :: l ->
          if List.length l > 0 then (
            let tail =
              if String.length s > 0 then List.nth (string_to_list s) 0 else ""
            in
            let env =
              ("substr", Vstring s) :: ("tail", Vstring tail)
              :: ("first", Vbool first) :: ("max", Vint max)
              :: ("cnt", Vint cnt) :: env
            in
            List.iter (print_ast env xx) al;
            loop false (cnt + 1) l)
          else () (* dont do last element *)
      | [] -> ()
    in
    loop true 0 list_of_sub
  and print_foreach_value env xx al =
    let l =
      match get_env "list_value" env with
      | Vlist_value l ->
          List.sort
            (fun (_, s1) (_, s2) ->
              let rss1 = Place.without_suburb s1 in
              let rss2 = Place.without_suburb s2 in
              if rss1 = rss2 then Gutil.alphabetic_order s1 s2
              else Gutil.alphabetic_order rss1 rss2)
            l
      | _ -> []
    in
    let max = List.length l in
    List.iteri
      (fun i (k, s) ->
        let env =
          ("cnt", Vint i) :: ("max", Vint max) :: ("entry_value", Vstring s)
          :: ("entry_value_rev", Vstring (unfold_place_long false s))
          :: ("entry_key", Vstring (string_of_istr k))
          :: env
        in
        List.iter (print_ast env xx) al)
      l
  and print_foreach_initial env xx al =
    let l = match get_env "list" env with Vlist_data l -> l | _ -> [] in
    let ini_l = build_list_short conf l in
    List.iter
      (fun ini ->
        let env = ("ini", Vstring ini) :: env in
        List.iter (print_ast env xx) al)
      ini_l
  in
  print_foreach

let print_mod conf base =
  match p_getenv conf.env "data" with
  | Some ("place" | "src" | "occu" | "fn" | "sn") ->
      let list = build_list conf base in
      let env = [ ("list", Vlist_data list); ("count", Vcnt (ref 0)) ] in
      Hutil.interp conf "upddata"
        {
          Templ.eval_var = eval_var conf base;
          Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
          Templ.eval_predefined_apply = (fun _ -> raise Not_found);
          Templ.get_vother;
          Templ.set_vother;
          Templ.print_foreach = print_foreach conf;
        }
        env ()
  | _ ->
      Hutil.interp conf "upddatamenu"
        {
          Templ.eval_var = (fun _ -> raise Not_found);
          Templ.eval_transl = (fun _ -> Templ.eval_transl conf);
          Templ.eval_predefined_apply = (fun _ -> raise Not_found);
          Templ.get_vother;
          Templ.set_vother;
          Templ.print_foreach = (fun _ -> raise Not_found);
        }
        [] ()

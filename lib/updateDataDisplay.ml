open Config
open Util
open UpdateData

let src = Logs.Src.create ~doc:"UpdateDataDisplay" "UPDT"

module Log = (val Logs.src_log src : Logs.LOG)
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Connection = Geneweb_http.Connection

let translate_title conf len =
  let plural = if len > 1 then 1 else 0 in
  let book_name =
    match p_getenv conf.env "data" with
    | Some "occu" -> transl_nth conf "occupation/occupations" plural
    | Some "place" -> transl_nth conf "place/places" plural
    | Some "src" -> transl_nth conf "source/sources" plural
    | Some "fn" -> transl_nth conf "first name/first names" plural
    | Some "sn" -> transl_nth conf "surname/surnames" plural
    | Some "fna" -> transl_nth conf "first name alias" plural
    | Some "sna" -> transl_nth conf "surname alias" plural
    | Some "alias" -> transl_nth conf "alias/aliases" plural
    | Some "qual" -> transl_nth conf "qualifier/qualifiers" plural
    | Some "pubn" -> transl_nth conf "public name/public names" plural
    | Some "title" -> transl_nth conf "title/titles" plural
    | Some "domain" -> transl_nth conf "domain/domains" plural
    | _ -> "unknown"
  in
  (Printf.sprintf (ftransl conf "book of %s") book_name, book_name)

let thousands conf n =
  Sosa.to_string_sep (transl conf "(thousand separator)") (Sosa.of_int n)

let print_list conn conf base =
  let print conf =
    Perso.interp_templ "list" conf base
      (Driver.empty_person base Driver.Iper.dummy)
  in
  match
    (p_getenv conf.env "data", p_getenv conf.env "key", p_getenv conf.env "i0")
  with
  | Some data, Some _, None -> (
      match get_person_from_data conf base with
      | [ (_, [ p ]) ] ->
          let url =
            if conf.wizard && (data = "title" || data = "domain") then
              Printf.sprintf "%sm=MOD_IND&i=%s#titles"
                (commd conf :> string)
                (Driver.Iper.to_string (Driver.get_iper p))
            else (Adef.(commd conf ^^^ Util.acces conf base p) :> string)
          in
          Connection.http_redirect_temporarily conn url
      | (istr, pl) :: _ ->
          let ipers =
            List.mapi
              (fun i p ->
                ( Printf.sprintf "i%d" i,
                  Adef.encoded (Driver.Iper.to_string (Driver.get_iper p)) ))
              pl
          in
          print
            {
              conf with
              env =
                ("nb", Adef.encoded (string_of_int (List.length pl)))
                :: ("k", Mutil.encode (Driver.sou base istr))
                :: List.rev_append ipers conf.env;
            }
      | [] -> print conf)
  | _ -> print conf

let print_json conf json =
  Output.header conf "Content-type: application/json";
  Output.header conf "Connection: close";
  Output.print_sstring conf (Yojson.Basic.to_string json);
  Output.flush conf

(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)

(* ******************************************************************** *)

(** [Description] : Met à jour toutes les personnes en relation avec la donnée
    que l'on veut modifié. [Args] :
    - conf : configuration
    - base : base [Retour] :
    - unit [Rem] : Non exporté en clair hors de ce module. *)
let print_mod_ok conf base =
  (* [direct_istr=on]: rewrite the string in place. When the new string
     already exists [replace_string] changes nothing and returns its id; fall
     back to the normal update, which moves the references to it. *)
  let replaced_in_place () =
    let old_istr =
      Driver.Istr.of_string @@ (List.assoc "key" conf.env :> string)
    in
    let old_string = Driver.sou base old_istr in
    let new_string =
      Option.fold ~none:"" ~some:only_printable (p_getenv conf.env "nx_input")
    in
    Driver.Istr.equal old_istr
      (Driver.replace_string base old_string new_string)
  in
  if p_getenv conf.env "direct_istr" = Some "on" && replaced_in_place () then
    Util.commit_patches conf base
  else
    let sn = p_getenv conf.env "data" = Some "sn" in
    let ini_of_update_data ini new_input =
      (* Utf8. returns the number of "real" characters *)
      let len = Utf8.length ini in
      let len = if len = 1 then len + 1 else len in
      let len = min len (Utf8.length new_input) in
      let new_input =
        if sn then Util.surname_without_particle base new_input else new_input
      in
      let j =
        let rec loop i j =
          if i = len then j else loop (i + 1) (Utf8.next new_input j)
        in
        loop 0 0
      in
      String.sub new_input 0 j
    in
    let data = Option.value ~default:"" (p_getenv conf.env "data") in
    let ini = Option.value ~default:"" (p_getenv_notrim conf.env "s") in
    let new_input =
      Option.fold ~none:"" ~some:only_printable (p_getenv conf.env "nx_input")
    in
    let new_istr = Driver.insert_string base new_input in
    let new_istr_s = Driver.Istr.to_string new_istr in
    let ajax = p_getenv conf.env "ajax" <> None in
    let new_ini = ini_of_update_data ini (nav_key conf base new_input) in
    let o_param = if rev_param conf then "&o=r" else "" in
    let print_o () = Output.print_sstring conf o_param in
    let list = get_person_from_data conf base in
    let list =
      List.map (fun (istr, perl) -> (Driver.sou base istr, perl)) list
    in
    let nb_pers =
      List.fold_left (fun accu (_, perl) -> accu + List.length perl) 0 list
    in
    let data_modified = List.for_all (fun (old, _) -> new_input <> old) list in
    if nb_pers <> 0 && data_modified then (
      let nb = update_person_list conf base new_input list in
      if ajax then
        print_json conf
          (`Assoc
             [
               ("success", `Bool true);
               ( "label",
                 `String
                   (Printf.sprintf "%s%s %d %s"
                      (Utf8.capitalize_fst
                         (transl conf "modification successful"))
                      (transl conf ":") nb
                      (transl_nth conf "modification/modifications"
                         (if nb > 1 then 1 else 0))) );
               ( "before",
                 `String (match list with (s, _) :: _ -> s | [] -> "") );
               ("after", `String new_input);
               ("key", `String new_istr_s);
               ( "entry",
                 match book_entry conf base new_istr with
                 | Some (_, s) -> `String s
                 | None -> `Null );
               ( "url",
                 `String
                   (Printf.sprintf "%sm=MOD_DATA&data=%s&s=%s%s#k%s"
                      (commd conf :> string)
                      (Mutil.encode data :> string)
                      (Mutil.encode new_ini :> string)
                      o_param new_istr_s) );
             ])
      else
        let title _ =
          transl conf "modification successful"
          |> Utf8.capitalize_fst |> Output.print_sstring conf
        in
        Hutil.header conf title;
        Output.print_sstring conf "<p>";
        transl conf "modification successful"
        |> Utf8.capitalize_fst |> Output.print_sstring conf;
        Output.print_sstring conf (transl conf ":");
        Output.print_sstring conf " ";
        Output.print_sstring conf (string_of_int nb);
        Output.print_sstring conf " ";
        if List.assoc_opt "history" conf.base_env = Some "yes" then (
          Output.print_sstring conf "<a href=\"";
          Output.print_string conf (commd conf);
          Output.print_sstring conf "m=HIST&k=20\">";
          Output.print_sstring conf
            (transl_nth conf "modification/modifications"
               (if nb > 1 then 1 else 0));
          Output.print_sstring conf ".</a>")
        else (
          Output.print_sstring conf
            (transl_nth conf "modification/modifications"
               (if nb > 1 then 1 else 0));
          Output.print_sstring conf ".");
        Output.print_sstring conf "</p>";
        Output.print_sstring conf {|<a href="|};
        Output.print_string conf (commd conf);
        Output.print_sstring conf {|m=MOD_DATA&data=|};
        Output.print_string conf (Mutil.encode data);
        Output.print_sstring conf {|&s=|};
        Output.print_string conf (Mutil.encode new_ini);
        print_o ();
        Output.print_sstring conf ("#k" ^ new_istr_s);
        Output.print_sstring conf {|" id="reference">|};
        Output.print_sstring conf
          (Utf8.capitalize_fst (transl conf "new modification"));
        Output.print_sstring conf " ";
        Output.print_sstring conf (transl conf "at new location");
        Output.print_sstring conf {|</a>|};
        if not (String.starts_with ~prefix:ini new_ini) then (
          Output.print_sstring conf {| / <a href="|};
          Output.print_string conf (commd conf);
          Output.print_sstring conf {|m=MOD_DATA&data=|};
          Output.print_string conf (Mutil.encode data);
          Output.print_sstring conf {|&s=|};
          Output.print_string conf (Mutil.encode ini);
          print_o ();
          Output.print_sstring conf {|">|};
          Output.print_sstring conf (transl conf "at old location");
          Output.print_sstring conf {|</a>|});
        Output.print_sstring conf ".";
        Hutil.trailer conf)
    else if ajax then
      print_json conf
        (`Assoc
           [
             ("success", `Bool false);
             ( "message",
               `String (Utf8.capitalize_fst (transl conf "no modification")) );
           ])
    else (
      Hutil.header conf (fun _ ->
          transl conf "no modification"
          |> Utf8.capitalize_fst |> Output.print_sstring conf);
      Output.print_sstring conf {|<p><a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=MOD_DATA&data=|};
      Output.print_string conf (Mutil.encode data);
      Output.print_sstring conf {|&s=|};
      Output.print_string conf (Mutil.encode ini);
      print_o ();
      Output.print_sstring conf ("#k" ^ new_istr_s);
      Output.print_sstring conf {|" id="reference">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl conf "new modification"));
      Output.print_sstring conf {|</a></p>|};
      Hutil.trailer conf)

type 'a env =
  | Vbool of bool
  | Vint of int
  | Vlist_data of book
  | Vlist_value of (Driver.istr * string) list
  | Vnone
  | Vother of 'a
  | Vstring of string

let visual_spaces_html s =
  let buffer = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      if c = ' ' then Buffer.add_string buffer "<span class=\"sp-ul\"> </span>"
      else Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let unfold_place_long inverted s =
  let pl, sub = Place.fold_place_long inverted s in
  if inverted then String.concat ", " (List.rev (sub :: List.rev pl))
  else String.concat ", " (sub :: pl)

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x
let bool_val x = Templ.VVbool x
let str_val x = Templ.VVstring x

let rec eval_var conf base env xx _loc sl =
  try eval_simple_var conf base env xx sl
  with Not_found -> eval_compound_var conf base env xx sl

and eval_simple_var conf base env xx = function
  | [ "rev_order" ] -> Templ.VVbool (rev_order conf base)
  | [ "permalink" ] ->
      Templ.VVstring
        (Permalink.script conf (Permalink.query conf base) :> string)
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
  | "new_head" -> (
      match get_env "new_head" env with Vbool x -> x | _ -> false)
  | "script" -> ( match get_env "script" env with Vbool x -> x | _ -> false)
  | "entry_blank" -> Name.lower (eval_string_env "entry_value" env) = ""
  | _ -> raise Not_found

and eval_simple_str_var conf base env _xx = function
  | "cnt" -> eval_int_env "cnt" env
  | "columns" -> (
      match (get_env "list" env, p_getenv conf.env "data") with
      | _, Some "src" -> ""
      | Vlist_data b, data ->
          Util.columns_style
            ~wide:
              (List.mem data
                 [ Some "place"; Some "occu"; Some "title"; Some "domain" ])
            (b.count + (3 * List.length b.groups))
      | _ -> "")
  | "current_s" -> Option.value ~default:"" (p_getenv_notrim conf.env "s")
  | "entry_count" -> eval_int_env "entry_count" env
  | "entry_label" -> (
      match get_env "entry_count" env with
      | Vint n ->
          (Util.escape_html
             (Printf.sprintf
                (if rev_order conf base then
                   ftransl conf "%s %s whose last term starts with %s"
                 else ftransl conf "%s %s starting with %s")
                (thousands conf n)
                (snd (translate_title conf n))
                (eval_string_env "entry_ini" env))
            :> string)
      | _ -> raise Not_found)
  | "entry_nb" -> (
      match get_env "entry_count" env with
      | Vint n -> thousands conf n
      | _ -> raise Not_found)
  | "entry_ini" -> eval_string_env "entry_ini" env
  | "entry_ini_display" -> eval_string_env "entry_ini_display" env
  | "entry_value" -> eval_string_env "entry_value" env
  | "entry_value_rev" -> eval_string_env "entry_value_rev" env
  | "rev_head" -> eval_string_env "rev_head" env
  | "entry_key" -> eval_string_env "entry_key" env
  | "ini" -> eval_string_env "ini" env
  | "max" -> eval_int_env "max" env
  | "nb_results" -> (
      match get_env "list" env with
      | Vlist_data b -> string_of_int b.count
      | _ -> "0")
  | "other" -> (
      match p_getenv conf.env "data" with
      | Some "place" -> Place.without_suburb (eval_string_env "entry_value" env)
      | _ -> "")
  | "script_class" -> eval_string_env "script_class" env
  | "script_title" -> eval_string_env "script_title" env
  | "suburb" -> (
      match p_getenv conf.env "data" with
      | Some "place" -> Place.only_suburb (eval_string_env "entry_value" env)
      | _ -> "")
  | "substr" -> eval_string_env "substr" env
  | "tail" -> eval_string_env "tail" env
  | "title" ->
      let book_of, _ = translate_title conf 2 in
      (* book of is always plural *)
      Utf8.capitalize_fst book_of
  | "subtitle" -> (
      let len =
        match get_env "list" env with Vlist_data b -> b.count | _ -> 0
      in
      let len2 = thousands conf len in
      let _, book_name = translate_title conf len in
      match p_getenv_notrim conf.env "s" with
      | Some ini ->
          if ini = "" then Printf.sprintf "%s %s" len2 book_name
          else
            let fmt_str =
              if rev_order conf base then
                ftransl conf "%s %s whose last term starts with %s"
              else ftransl conf "%s %s starting with %s"
            in
            let escaped_ini =
              Printf.sprintf "<span>%s</span>"
                (visual_spaces_html (Util.escape_html ini :> string))
            in
            Printf.sprintf fmt_str len2 book_name escaped_ini
      | None -> Printf.sprintf "%s %s" len2 book_name)
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
    | "visual_spaces" :: sl -> visual_spaces_html (loop sl)
    | [ "entry_value"; "original" ] ->
        let istr_string = eval_string_env "entry_key" env in
        let istr = Driver.Istr.of_string istr_string in
        Driver.sou base istr
    | "encode" :: sl -> Util.uri_encode (loop sl)
    | "escape" :: sl ->
        let escaped = Util.escape_html (loop sl) in
        (escaped :> string)
    | [ "subs"; n; s ] -> (
        match int_of_string_opt n with
        | Some n ->
            if String.length s > n then String.sub s 0 (String.length s - n)
            else (
              Log.warn (fun k -> k "String shorter that requested");
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

let print_foreach conf base print_ast _eval_expr =
  let rec print_foreach env xx _loc s sl el al =
    match s :: sl with
    | [ "initial" ] -> print_foreach_initial env xx al
    | [ "entry" ] -> print_foreach_entry env xx el al
    | [ "substring"; e; n ] -> print_foreach_substring env xx el al e n
    | [ "value" ] -> print_foreach_value env xx al
    | _ -> raise Not_found
  and print_foreach_entry env xx _el al =
    let list =
      match get_env "list" env with Vlist_data b -> b.groups | _ -> []
    in
    let max = List.length list in
    let k = Option.value ~default:"" (p_getenv conf.env "key") in
    let current_s = Option.value ~default:"" (p_getenv_notrim conf.env "s") in

    List.iteri
      (fun i (ini_k, n, list_v) ->
        let display_ini =
          let should_truncate =
            (String.length current_s > 6 && max > 6)
            || (String.length current_s > 4 && max > 10)
          in
          if should_truncate && String.length ini_k > String.length current_s
          then
            let suffix =
              String.sub ini_k (String.length current_s)
                (String.length ini_k - String.length current_s)
            in
            if String.length current_s >= 2 then
              let utf8_len = Utf8.length current_s in
              if utf8_len >= 2 then
                let last_two = Utf8.sub current_s (utf8_len - 2) 2 in
                "…" ^ last_two ^ suffix
              else ini_k
            else ini_k
          else ini_k
        in
        let script_info =
          if String.length ini_k > 0 then
            let first_char = Utf8.C.cp ini_k 0 in
            match Uucp.Script.script first_char with
            | `Arab -> Some ("arab-char", "Arabic")
            | `Armn -> Some ("armn-char", "Armenian")
            | `Cyrl -> Some ("cyrl-char", "Cyrillic")
            | `Ethi -> Some ("ethi-char", "Ethiopic")
            | `Geor -> Some ("geor-char", "Georgian")
            | `Grek -> Some ("grek-char", "Greek")
            | `Hebr -> Some ("hebr-char", "Hebrew")
            | `Khmr -> Some ("khmr-char", "Khmer")
            | _ -> None
          else None
        in
        let env =
          Templ.Env.(
            env |> add "cnt" (Vint i) |> add "max" (Vint max)
            |> add "key" (Vstring k)
            |> add "entry_ini" (Vstring ini_k)
            |> add "entry_ini_display" (Vstring display_ini)
            |> add "script" (Vbool (Option.is_some script_info))
            |> add "script_class"
                 (Vstring (Option.fold ~none:"" ~some:fst script_info))
            |> add "script_title"
                 (Vstring (Option.fold ~none:"" ~some:snd script_info))
            |> add "entry_count" (Vint n)
            |> add "list_value" (Vlist_value list_v))
        in
        List.iter (print_ast env xx) al)
      list
  and print_foreach_substring env xx _el al evar n =
    let evar =
      match p_getenv_notrim conf.env evar with Some s -> s | None -> ""
    in
    let limit = match int_of_string_opt n with Some i -> i | None -> 12 in
    let rec build_substrings i acc =
      if i >= Utf8.length evar || i > limit then List.rev acc
      else
        let substr = Utf8.sub evar 0 i in
        build_substrings (i + 1) (substr :: acc)
    in
    let substrings = build_substrings 1 [] in
    let max = List.length substrings in
    let rec process_items idx = function
      | [] -> ()
      | s :: tail ->
          let env =
            Templ.Env.(
              env |> add "substr" (Vstring s) |> add "max" (Vint max)
              |> add "cnt" (Vint idx))
          in
          List.iter (print_ast env xx) al;
          process_items (idx + 1) tail
    in

    process_items 0 substrings
  and print_foreach_value env xx al =
    let alphabetic (_, s1) (_, s2) =
      let rss1 = Place.without_suburb s1 in
      let rss2 = Place.without_suburb s2 in
      if rss1 = rss2 then Gutil.alphabetic_order s1 s2
      else Gutil.alphabetic_order rss1 rss2
    in
    (* in the order from the last term, a heading before each last term *)
    let l =
      match get_env "list_value" env with
      | Vlist_value l when rev_order conf base ->
          List.rev_map (fun ((_, s) as x) -> (Dict.last_terms s, x)) l
          |> List.sort (fun (t1, x1) (t2, x2) ->
              match compare_terms t1 t2 with 0 -> alphabetic x1 x2 | c -> c)
          |> List.map (fun (t, (k, s)) ->
              (k, s, match t with [] -> "" | t :: _ -> t))
      | Vlist_value l ->
          List.sort alphabetic l |> List.map (fun (k, s) -> (k, s, ""))
      | _ -> []
    in
    let max = List.length l in
    let rec loop i l prev prev_head =
      match l with
      | [] -> ()
      | (k, s, head) :: l ->
          let env =
            Templ.Env.(
              env |> add "cnt" (Vint i) |> add "max" (Vint max)
              |> add "entry_value" (Vstring s)
              |> add "entry_value_rev" (Vstring (unfold_place_long false s))
              |> add "entry_key" (Vstring (Driver.Istr.to_string k))
              |> add "first" (Vbool (Place.without_suburb s <> prev))
              |> add "rev_head" (Vstring head)
              |> add "new_head" (Vbool (head <> "" && head <> prev_head)))
          in
          List.iter (print_ast env xx) al;
          loop (i + 1) l (Place.without_suburb s) head
    in
    loop 0 l "" ""
  and print_foreach_initial env xx al =
    let ini_l =
      match get_env "list" env with
      | Vlist_data b -> (
          match
            List.fold_left
              (fun acc (_, _, l) -> List.rev_append l acc)
              [] b.groups
          with
          | [] -> List.map (fun (ini, _, _) -> ini) b.groups
          | l -> build_list_short conf base l)
      | _ -> []
    in
    List.iter
      (fun ini ->
        let env = Templ.Env.add "ini" (Vstring ini) env in
        List.iter (print_ast env xx) al)
      ini_l
  in
  print_foreach

let print_mod conf base =
  let env =
    Templ.Env.empty |> Templ.Env.add "list" (Vlist_data (book conf base))
  in
  let ifun =
    Templ.
      {
        eval_var = eval_var conf base;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = print_foreach conf base;
      }
  in
  Templ.output conf ifun env () "upddata"

open Config
open Util
open UpdateData
module Logs = Geneweb_logs.Logs
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver

let translate_title conf len =
  let plural = if len > 1 then 1 else 0 in
  let book_name =
    match p_getenv conf.env "data" with
    | Some "occu" -> transl_nth conf "occupation/occupations" plural
    | Some "place" -> transl_nth conf "place/places" plural
    | Some "src" -> transl_nth conf "source/sources" plural
    | Some "fn" -> transl_nth conf "first name/first names" plural
    | Some "sn" -> transl_nth conf "surname/surnames" plural
    | Some "alias" -> transl_nth conf "alias/aliases" plural
    | Some "qual" -> transl_nth conf "qualifier/qualifiers" plural
    | Some "pubn" -> transl_nth conf "public name/public names" plural
    | Some "title" -> transl_nth conf "title/titles" plural
    | Some "domain" -> transl_nth conf "domain/domains" plural
    | _ -> "unknown"
  in
  (Printf.sprintf (ftransl conf "book of %s") book_name, book_name)

(* ******************************************************************** *)
(*  [Fonc] print_mod_ok : config -> base -> unit                        *)

(* ******************************************************************** *)

(** [Description] : Met à jour toutes les personnes en relation avec la donnée
    que l'on veut modifié. [Args] :
    - conf : configuration
    - base : base [Retour] :
    - unit [Rem] : Non exporté en clair hors de ce module. *)
let print_mod_ok conf base =
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
  let ini = Option.value ~default:"" (p_getenv conf.env "s") in
  let new_input =
    Option.fold ~none:"" ~some:only_printable (p_getenv conf.env "nx_input")
  in
  let new_istr_s =
    Driver.Istr.to_string (Driver.insert_string base new_input)
  in
  let new_ini = ini_of_update_data ini new_input in
  let list = get_person_from_data conf base in
  let list = List.map (fun (istr, perl) -> (Driver.sou base istr, perl)) list in
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
        if n > 5000 then 5000 else n
    | _ -> 5000
  in
  if nb_pers <> 0 && data_modified then (
    let nb_pers_reel =
      update_person_list conf base new_input list nb_pers max_updates
    in
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
    Output.print_sstring conf (min nb_pers_reel max_updates |> string_of_int);
    Output.print_sstring conf " ";
    if List.assoc_opt "history" conf.base_env = Some "yes" then (
      Output.print_sstring conf "<a href=\"";
      Output.print_string conf (commd conf);
      Output.print_sstring conf "m=HIST&k=20\">";
      Output.print_sstring conf
        (transl_nth conf "modification/modifications"
           (if nb_pers_reel > 1 then 1 else 0));
      Output.print_sstring conf ".</a>")
    else (
      Output.print_sstring conf
        (transl_nth conf "modification/modifications"
           (if nb_pers_reel > 1 then 1 else 0));
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
        {|<button type="submit" class="btn btn-primary btn-lg">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
      Output.print_sstring conf "</button></p></form>");
    Output.print_sstring conf {|<a href="|};
    Output.print_string conf (commd conf);
    Output.print_sstring conf {|m=MOD_DATA&data=|};
    Output.print_string conf (Mutil.encode data);
    Output.print_sstring conf {|&s=|};
    Output.print_string conf (Mutil.encode new_ini);
    Output.print_sstring conf ("#k" ^ new_istr_s);
    Output.print_sstring conf {|" id="reference">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "new modification"));
    Output.print_sstring conf " ";
    Output.print_sstring conf (transl conf "at new location");
    Output.print_sstring conf {|</a>|};
    if not (Mutil.start_with ini 0 new_ini) then (
      Output.print_sstring conf {| / <a href="|};
      Output.print_string conf (commd conf);
      Output.print_sstring conf {|m=MOD_DATA&data=|};
      Output.print_string conf (Mutil.encode data);
      Output.print_sstring conf {|&s=|};
      Output.print_string conf (Mutil.encode ini);
      Output.print_sstring conf {|">|};
      Output.print_sstring conf (transl conf "at old location");
      Output.print_sstring conf {|</a>|});
    Output.print_sstring conf ".";
    Hutil.trailer conf)
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
    Output.print_sstring conf ("#k" ^ new_istr_s);
    Output.print_sstring conf {|" id="reference">|};
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl conf "new modification"));
    Output.print_sstring conf {|</a></p>|};
    Hutil.trailer conf)

type 'a env =
  | Vbool of bool
  | Vcnt of int ref
  | Vint of int
  | Vlist_data of (Driver.istr * string) list
  | Vlist_value of (Driver.istr * string) list
  | Vnone
  | Vother of 'a
  | Vstring of string

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
  | [ "p_list" ] ->
      let data =
        match p_getenv conf.env "data" with Some data -> data | _ -> ""
      in
      let istr =
        match get_env "key" env with Vstring istr -> istr | _ -> "0"
      in
      let conf =
        {
          conf with
          env =
            ("key", Adef.encoded istr)
            :: ("data", Adef.encoded data)
            :: conf.env;
        }
      in
      let istr, p_list = List.hd (get_person_from_data conf base) in
      (* same code as in place.ml *)
      let head =
        Printf.sprintf "<a href=\"%sm=L%s%s&k=%s&nb=%d"
          (commd conf :> string)
          (Format.sprintf "&data=%s" data)
          "&bi=on&ba=on&ma=on&de=on&bu=on&parents=0"
          (Mutil.encode (Driver.sou base istr) :> string)
          (List.length p_list)
      in
      let body =
        let rec loop i acc = function
          | [] -> acc
          | p :: pl ->
              let ip = Driver.get_iper p in
              loop (i + 1)
                (acc ^ Printf.sprintf "&i%d=%s" i (Driver.Iper.to_string ip))
                pl
        in
        loop 0 "" p_list
      in
      let title =
        Printf.sprintf
          "\" title=\"%s\" data-toggle=\"tooltip\" data-placement=\"top\">%d"
          (Utf8.capitalize (transl conf "list of linked persons"))
          (List.length p_list)
      in
      let tail = Printf.sprintf "<i class=\"fa fa-user fa-xs ml-1\"></i></a>" in
      Printf.sprintf "%s%s%s%s" head body title tail |> str_val
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
  | "entry_value_unsort" ->
      let sn = p_getenv conf.env "data" = Some "sn" in
      let s = eval_string_env "entry_value" env in
      if sn && s.[String.length s - 1] = ')' then
        match String.rindex_opt s '(' with
        | Some i ->
            let part = String.sub s (i + 1) (String.length s - i - 2) in
            let part =
              if
                part.[String.length part - 1] = '\''
                || String.length part >= 3
                   && Char.code part.[String.length part - 3] = 0xE2
                   && Char.code part.[String.length part - 2] = 0x80
                   && (Char.code part.[String.length part - 1] = 0x98
                      || Char.code part.[String.length part - 1] = 0x99)
              then part
              else part ^ " "
            in
            part ^ String.sub s 0 i
        | _ -> s
      else s
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
      let book_of, _ = translate_title conf 2 in
      (* book of is always plural *)
      Utf8.capitalize_fst book_of
  | "subtitle" -> (
      let len =
        match get_env "list" env with Vlist_data l -> List.length l | _ -> 0
      in
      let len2 =
        Sosa.to_string_sep
          (transl conf "(thousand separator)")
          (Sosa.of_int len)
      in
      let _, book_name = translate_title conf len in
      match p_getenv conf.env "s" with
      | Some ini ->
          if ini = "" then Printf.sprintf "%s %s" len2 book_name
          else
            let fmt_str = ftransl conf "%s %s starting with %s" in
            let escaped_ini =
              Printf.sprintf "<span class=\"hl-book\">%s</span>"
                (Util.escape_html ini :> string)
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
    | "encode" :: sl -> Util.uri_encode (loop sl)
    | "escape" :: sl ->
        let escaped = Util.escape_html (loop sl) in
        (escaped :> string)
    | [ "subs"; n; s ] -> (
        match int_of_string_opt n with
        | Some n ->
            if String.length s > n then String.sub s 0 (String.length s - n)
            else (
              Logs.syslog `LOG_WARNING "String shorter that requested\n";
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
    | [ "substring"; e; n ] -> print_foreach_substring env xx el al e n
    | [ "value" ] -> print_foreach_value env xx al
    | _ -> raise Not_found
  and print_foreach_entry env xx _el al =
    let list = match get_env "list" env with Vlist_data l -> l | _ -> [] in
    let list = build_list_long conf list in
    let max = List.length list in
    let k = Option.value ~default:"" (p_getenv conf.env "key") in
    List.iteri
      (fun i (ini_k, (list_v : (Driver.istr * string) list)) ->
        let env =
          Templ.Env.(
            env |> add "cnt" (Vint i) |> add "max" (Vint max)
            |> add "key" (Vstring k)
            |> add "entry_ini" (Vstring ini_k)
            |> add "list_value" (Vlist_value list_v))
        in
        List.iter (print_ast env xx) al)
      list
  and print_foreach_substring env xx _el al evar n =
    let evar = match p_getenv conf.env evar with Some s -> s | None -> "" in
    (* Parse the limiting integer from n *)
    let limit =
      match int_of_string_opt n with
      | Some i -> i
      | None -> 12 (* Default limit if n is not a valid integer *)
    in

    (* Generate the list of progressive substrings *)
    let rec build_substrings i acc =
      if i >= Utf8.length evar || i > limit then List.rev acc
      else
        let rec get_substr j pos =
          if j >= i then pos else get_substr (j + 1) (Utf8.next evar pos)
        in
        let substr = String.sub evar 0 (get_substr 0 0) in
        build_substrings (i + 1) (substr :: acc)
    in

    let substrings = build_substrings 1 [] in
    let max = List.length substrings in

    (* Process each substring *)
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
    let rec loop i l prev =
      match l with
      | [] -> ()
      | (k, s) :: l ->
          let env =
            Templ.Env.(
              env |> add "cnt" (Vint i) |> add "max" (Vint max)
              |> add "entry_value" (Vstring s)
              |> add "entry_value_rev" (Vstring (unfold_place_long false s))
              |> add "entry_key" (Vstring (Driver.Istr.to_string k))
              |> add "first" (Vbool (Place.without_suburb s <> prev)))
          in
          List.iter (print_ast env xx) al;
          loop (i + 1) l (Place.without_suburb s)
    in
    loop 0 l ""
  and print_foreach_initial env xx al =
    let l = match get_env "list" env with Vlist_data l -> l | _ -> [] in
    let ini_l = build_list_short conf l in
    List.iter
      (fun ini ->
        let env = Templ.Env.add "ini" (Vstring ini) env in
        List.iter (print_ast env xx) al)
      ini_l
  in
  print_foreach

let print_mod conf base =
  let list = build_list conf base in
  let env =
    Templ.Env.(
      empty |> add "list" (Vlist_data list) |> add "count" (Vcnt (ref 0)))
  in
  let ifun =
    Templ.
      {
        eval_var = eval_var conf base;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = print_foreach conf;
      }
  in
  Templ.output conf ifun env () "upddata"

open Def
open Config
open Gwdb
open Util

let default_max_cnt = Alln.default_max_cnt

(* tools *)

let particle_at_the_end base is_surnames s =
  if is_surnames then
    surname_without_particle base s ^ surname_particle base s
  else s

let compare_particle_at_the_end base is_surnames a b =
  Gutil.alphabetic_order
    (particle_at_the_end base is_surnames a)
    (particle_at_the_end base is_surnames b)

(* print *)

let print_title conf base is_surnames ini len =
  if len >= 2 then
    if is_surnames
    then
      Printf.sprintf (fcapitale (ftransl conf "the %d surnames")) len
      |> Output.print_sstring conf
    else
      Printf.sprintf (fcapitale (ftransl conf "the %d first names")) len
      |> Output.print_sstring conf
  else if is_surnames then
    transl_nth conf "surname/surnames" 0
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf
  else
    transl_nth conf "first name/first names" 0
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
  if ini <> "" then begin
    Output.print_sstring conf " " ;
    Output.print_sstring conf (transl conf "starting with") ;
    Output.print_sstring conf " " ;
    Output.print_string conf (Util.escape_html ini)
  end else begin
    Output.print_sstring conf " (" ;
    Output.print_sstring conf (string_of_int @@ Gwdb.nb_of_real_persons base) ;
    Output.print_sstring conf " " ;
    Output.print_sstring conf (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1)) ;
    Output.print_sstring conf ")" ;
  end

let tr c1 s2 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
  in
  loop 0 0

let print_alphabetic_big conf base is_surnames ini list len too_big =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then Adef.encoded "N" else Adef.encoded "P" in
  Hutil.header conf title;
  Output.print_sstring conf {|<p class="search_name">|};
  List.iter begin fun ini_k ->
    if ini_k = ini
    then begin
      Output.print_sstring conf {|<a href="|} ;
      Output.print_string conf (commd conf) ;
      Output.print_sstring conf "m=" ;
      Output.print_string conf mode ;
      Output.print_sstring conf "&tri=A&v=" ;
      Output.print_string conf (Mutil.encode ini_k) ;
      Output.print_sstring conf {|">|} ;
    end else begin
      Output.print_sstring conf {|<a href="|} ;
      Output.print_string conf (commd conf) ;
      Output.print_sstring conf "m=" ;
      Output.print_string conf mode ;
      Output.print_sstring conf "&tri=A&k=" ;
      Output.print_string conf (Mutil.encode ini_k) ;
      Output.print_sstring conf {|">|} ;
    end ;
    Output.print_string conf (tr '_' "&nbsp;" ini_k |> Util.escape_html);
    Output.print_sstring conf "</a>"
  end list ;
  if not too_big then begin
    Output.print_sstring conf "</p><p>";
    (transl conf "the whole list")
    |> Utf8.capitalize_fst
    |> Output.print_sstring conf ;
    Output.print_sstring conf (transl conf ":");
    Output.print_sstring conf "</p><ul><li>";
    Output.print_sstring conf {|<a href="|} ;
    Output.print_string conf (commd conf) ;
    Output.print_sstring conf "m=" ;
    Output.print_string conf mode ;
    Output.print_sstring conf "&tri=A&o=A&k=" ;
    Output.print_string conf (Mutil.encode ini) ;
    Output.print_sstring conf {|">|} ;
    Output.print_sstring conf (transl conf "long display");
    Output.print_sstring conf "</a></li><li>";
    Output.print_sstring conf {|<a href="|} ;
    Output.print_string conf (commd conf) ;
    Output.print_sstring conf "m=" ;
    Output.print_string conf mode ;
    Output.print_sstring conf "&tri=S&o=A&k=" ;
    Output.print_string conf (Mutil.encode ini) ;
    Output.print_sstring conf {|">|} ;
    Output.print_sstring conf (transl conf "short display");
    Output.print_sstring conf "</a></li><li>";
    Output.print_sstring conf {|<a href="|} ;
    Output.print_string conf (commd conf) ;
    Output.print_sstring conf "m=" ;
    Output.print_string conf mode ;
    Output.print_sstring conf "&tri=S&o=A&cgl=on&k=" ;
    Output.print_string conf (Mutil.encode ini) ;
    Output.print_sstring conf {|">|} ;
    Output.print_sstring conf (transl conf "short display") ;
    Output.print_sstring conf " + " ;
    Output.print_sstring conf (transl conf "cancel GeneWeb links") ;
    Output.print_sstring conf "</a></li></ul>" ;
  end ;
  Hutil.trailer conf

let print_alphabetic_all conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  Hutil.header conf title;
  Output.print_sstring conf {|<p class="search_name">|};
  List.iter begin fun (ini_k, _) ->
    Output.print_sstring conf "<a href=\"#a" ;
    Output.print_string conf (Mutil.encode ini_k) ;
    Output.print_sstring conf "\">" ;
    Output.print_string conf (Mutil.tr '_' ' ' ini_k |> Adef.safe);
    Output.print_sstring conf "</a> "
  end list;
  Output.print_sstring conf "</p><ul>";
  List.iter begin fun (ini_k, l) ->
    Output.print_sstring conf "<li><a id=\"a" ;
    Output.print_string conf (Mutil.encode ini_k) ;
    Output.print_sstring conf "\">" ;
    Output.print_string conf (Mutil.tr '_' ' ' ini_k |> Adef.safe);
    Output.print_sstring conf "</a><ul>";
    List.iter begin fun (s, cnt) ->
      Output.print_sstring conf "<li>";
      let href = "m=" ^<^ mode ^^^ "&v=" ^<^ Mutil.encode s ^>^ "&t=A" in
      wprint_geneweb_link conf
        (href :> Adef.escaped_string)
        ((particle_at_the_end base is_surnames s |> Util.escape_html) :> Adef.safe_string) ;
      Output.print_sstring conf " (" ;
      Output.print_sstring conf (string_of_int cnt) ;
      Output.print_sstring conf ")</li>"
    end (List.sort (fun (a, _) (b, _) -> compare_particle_at_the_end base is_surnames a b) l) ;
    Output.print_sstring conf "</ul></li>"
  end list;
  Output.print_sstring conf "</ul>";
  Hutil.trailer conf

let print_alphabetic_small conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  Hutil.header conf title;
  if list <> [] then begin
    Output.print_sstring conf "<ul>";
    List.iter begin fun (_, s, cnt) ->
      Output.print_sstring conf "<li>";
      Output.print_sstring conf "<a href=\"" ;
      Output.print_string conf (commd conf) ;
      Output.print_sstring conf "m=" ;
      Output.print_string conf mode ;
      Output.print_sstring conf "&v=" ;
      Output.print_string conf (Mutil.encode s);
      Output.print_sstring conf "&t=A\">" ;
      Output.print_string conf (particle_at_the_end base is_surnames s |> Util.escape_html);
      Output.print_sstring conf "</a> (";
      Output.print_sstring conf (string_of_int cnt);
      Output.print_sstring conf ")</li>"
    end
      (List.sort begin fun (_, a, _) (_, b, _) ->
          compare_particle_at_the_end base is_surnames a b
        end list);
      Output.print_sstring conf "</ul>"
    end;
  Hutil.trailer conf

let print_frequency_any conf base is_surnames list len =
  let title _ = print_title conf base is_surnames "" len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let n = ref 0 in
  Hutil.header conf title;
  Output.print_sstring conf "<ul>";
  List.iter begin fun (cnt, l) ->
    if !n <= default_max_cnt then begin
      Output.print_sstring conf "<li>";
      Output.print_sstring conf (string_of_int cnt);
      Output.print_sstring conf "<ul>";
      List.iter begin fun s ->
        Output.print_sstring conf "<li><a href=\"" ;
        Output.print_string conf (commd conf) ;
        Output.print_sstring conf "m=" ;
        Output.print_string conf mode ;
        Output.print_sstring conf "&v=" ;
        Output.print_string conf (Mutil.encode (Name.lower s));
        Output.print_sstring conf "\">" ;
        Output.print_string conf (particle_at_the_end base is_surnames s |> Util.escape_html);
        Output.print_sstring conf "</a></li>";
        incr n ;
      end l ;
      Output.print_sstring conf "</ul>" ;
      Output.print_sstring conf "</li>"
    end
  end list ;
  Output.print_sstring conf "</ul>" ;
  Hutil.trailer conf

let print_frequency conf base is_surnames =
  let () = load_strings_array base in
  let (list, len) = Alln.select_names conf base is_surnames "" max_int in
  let list = Alln.groupby_count list in
  print_frequency_any conf base is_surnames list len

let print_alphabetic conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    | Some k -> k
    | _ -> ""
  in
  if List.assoc_opt "fast_alphabetic" conf.base_env = Some "yes" && ini = ""
  then begin
    load_strings_array base ;
    let list = Alln.first_letters base is_surnames in
    let list = List.sort Gutil.alphabetic_order list in
    print_alphabetic_big conf base is_surnames ini list 1 true
  end else begin
    let all =
      match p_getenv conf.env "o" with
      | Some "A" -> true
      | _ -> false
    in
    if String.length ini < 2 then load_strings_array base ;
    let (list, len) =
      Alln.select_names conf base is_surnames ini (if all then max_int else 50)
    in
    match list with
    | Alln.Specify keys ->
      let keys = List.sort Gutil.alphabetic_order keys in
      let too_big = not all && List.length keys > Alln.default_max_cnt in
      print_alphabetic_big conf base is_surnames ini keys len too_big
    | Alln.Result list ->
      if len >= 50 || ini = "" then
        let list = Alln.groupby_ini (Utf8.length ini + 1) list in
        print_alphabetic_all conf base is_surnames ini list len
      else print_alphabetic_small conf base is_surnames ini list len
  end

(* short print *)

let print_alphabetic_short conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = Adef.encoded (if is_surnames then "N" else "P") in
  let need_ref = len >= 250 in
  Hutil.header conf title;
  if need_ref then begin
    Output.print_sstring conf "<p>";
    List.iter begin fun (ini_k, _) ->
      Output.print_sstring conf "<a href=\"" ;
      Output.print_string conf (Mutil.encode ini_k);
      Output.print_sstring conf "\">" ;
      Output.print_string conf (Mutil.tr '_' ' ' ini_k |> Util.escape_html);
      Output.print_sstring conf "</a> "
    end list;
    Output.print_sstring conf "</p>"
  end ;
  List.iter begin fun (ini_k, l) ->
    Output.print_sstring conf "<p>";
    Mutil.list_iter_first begin fun first (s, cnt) ->
      let href =
        " href=\"" ^<^ commd conf
        ^^^ ("m=" ^<^ mode ^^^ "&v=" ^<^ Mutil.encode s ^>^ "&t=A\"" :> Adef.escaped_string)
      in
      let name = Adef.encoded (if first && need_ref then " id=\"a" ^ ini_k ^ "\"" else "") in
      if not first then Output.print_sstring conf ", ";
      if (href :> string) <> "" || (name :> string) <> ""
      then begin
        Output.print_sstring conf "<a" ;
        Output.print_string conf href ;
        Output.print_string conf name ;
        Output.print_sstring conf ">" ;
      end ;
      Output.print_string conf (particle_at_the_end base is_surnames s |> Util.escape_html);
      if (href :> string) <> "" || (name :> string) <> "" then Output.print_sstring conf "</a>";
      Output.print_sstring conf " (" ;
      Output.print_sstring conf (string_of_int cnt) ;
      Output.print_sstring conf ")"
    end (List.sort (fun (a, _) (b, _) -> Gutil.alphabetic_order a b) l);
    Output.print_sstring conf "</p>"
  end list ;
  Hutil.trailer conf

let print_short conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    | Some k -> k
    | _ -> ""
  in
  let _ = if String.length ini < 2 then load_strings_array base in
  match Alln.select_names conf base is_surnames ini max_int with
  | Alln.Specify _, _ -> Hutil.incorrect_request conf
  | Alln.Result list, len ->
    let list = Alln.groupby_ini (Utf8.length ini + 1) list in
    print_alphabetic_short conf base is_surnames ini list len

(* main *)

let print_surnames conf base =
  match p_getenv conf.env "tri" with
    Some "F" -> print_frequency conf base true
  | Some "S" -> print_short conf base true
  | _ -> print_alphabetic conf base true

let print_first_names conf base =
  match p_getenv conf.env "tri" with
    Some "F" -> print_frequency conf base false
  | Some "S" -> print_short conf base false
  | _ -> print_alphabetic conf base false

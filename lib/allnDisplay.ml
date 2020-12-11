open Config
open Gwdb
open Util

let default_max_cnt = Alln.default_max_cnt

(* tools *)
let ini len k =
  let ini_k = Utf8.sub ~pad:'_' k 0 len in
  (* ini_k is "a fresh string": we can use unsafe. *)
  Mutil.unsafe_tr ' ' '_' ini_k

let particle_at_the_end base is_surnames s =
  if is_surnames then
    surname_without_particle base s ^ surname_particle base s
  else s

let compare_particle_at_the_end base is_surnames a b =
  Gutil.alphabetic_order
    (particle_at_the_end base is_surnames a)
    (particle_at_the_end base is_surnames b)

let groupby_ini len list =
  list
  |> Util.groupby
    ~key:(fun (k, _, _) -> ini len k)
    ~value:(fun (_, s, c) -> (s, c))
  |> List.sort (fun (a, _) (b, _) -> Gutil.alphabetic_order a b)

let groupby_count = function
  | Alln.Specify _ -> assert false
  | Alln.Result list ->
    list
    |> Util.groupby
      ~key:(fun (_, _, c) -> c)
      ~value:(fun (_, s, _) -> s)
    |> List.sort (fun (a, _) (b, _) -> compare b a)

(* print *)

let print_title conf base is_surnames ini len =
  if len >= 2 then
    if is_surnames then
      Output.printf conf (fcapitale (ftransl conf "the %d surnames")) len
    else Output.printf conf (fcapitale (ftransl conf "the %d first names")) len
  else if is_surnames then
    Output.print_string conf (Utf8.capitalize (transl_nth conf "surname/surnames" 0))
  else
    Output.print_string conf
      (Utf8.capitalize (transl_nth conf "first name/first names" 0));
  if ini <> "" then
    Output.printf conf " %s %s" (transl conf "starting with") ini
  else
    Output.printf conf " (%d %s)" (Gwdb.nb_of_real_persons base)
      (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1))

let displayify s = s
(* let rec loop i len =
 *   if i = String.length s then Buff.get len
 *   else
 *     let nbc = Name.nbc s.[i] in
 *     if nbc < 0 || i + nbc > String.length s then
 *       Buff.get (Buff.mstore len "...")
 *     else loop (i + nbc) (Buff.gstore len s i nbc)
 * in
 * loop 0 0 *)

let tr c1 s2 s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if String.unsafe_get s i = c1 then loop (i + 1) (Buff.mstore len s2)
    else loop (i + 1) (Buff.store len (String.unsafe_get s i))
  in
  loop 0 0

let print_alphabetic_big conf base is_surnames ini list len too_big =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  Output.printf conf "<p class=\"search_name\">\n";
  List.iter begin fun ini_k ->
    if ini_k = ini
    then
      Output.printf conf "<a href=\"%sm=%s&tri=A&v=%s\">" (commd conf) mode
        (Mutil.encode ini_k)
    else
      Output.printf conf "<a href=\"%sm=%s&tri=A&k=%s\">" (commd conf) mode
        (Mutil.encode ini_k);
    Output.print_string conf (tr '_' "&nbsp;" (displayify ini_k));
    Output.printf conf "</a>\n"
  end list ;
  if not too_big then begin
    Output.printf conf "</p>\n";
    Output.printf conf "<p>";
    Output.printf conf "%s:" (Utf8.capitalize (transl conf "the whole list"));
    Output.printf conf "</p>\n" ;
    Output.printf conf "<ul>\n";
    Output.printf conf "<li>";
    Output.printf conf "<a href=\"%sm=%s&tri=A&o=A&k=%s\">" (commd conf) mode ini;
    Output.print_string conf (transl conf "long display");
    Output.printf conf "</a>";
    Output.printf conf "</li>\n";
    Output.printf conf "<li>";
    Output.printf conf "<a href=\"%sm=%s&tri=S&o=A&k=%s\">" (commd conf) mode ini;
    Output.print_string conf (transl conf "short display");
    Output.printf conf "</a>";
    Output.printf conf "</li>\n";
    Output.printf conf "<li>";
    Output.printf conf "<a href=\"%sm=%s&tri=S&o=A&k=%s&cgl=on\">" (commd conf) mode ini;
    Output.printf conf "%s + %s" (transl conf "short display") (transl conf "cancel GeneWeb links");
    Output.printf conf "</a>";
    Output.printf conf "</li>\n";
    Output.printf conf "</ul>\n" ;
  end ;
  Hutil.trailer conf

let print_alphabetic_all conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  Output.printf conf "<p class=\"search_name\">\n";
  List.iter
    (fun (ini_k, _) ->
       let ini = ini_k in
       Output.printf conf "<a href=\"#a%s\">" ini;
       Output.print_string conf (Mutil.tr '_' ' ' ini);
       Output.printf conf "</a>\n")
    list;
  Output.printf conf "</p>\n";
  Output.printf conf "<ul>\n";
  List.iter
    (fun (ini_k, l) ->
       let ini = ini_k in
       Output.printf conf "<li>\n";
       Output.printf conf "<a id=\"a%s\">" ini_k;
       Output.print_string conf (Mutil.tr '_' ' ' ini);
       Output.printf conf "</a>\n";
       Output.printf conf "<ul>\n";
       List.iter
         (fun (s, cnt) ->
            Output.printf conf "<li>";
            begin let href =
                    "m=" ^ mode ^ "&v=" ^ Mutil.encode s ^ "&t=A"
              in
              wprint_geneweb_link conf href
                (particle_at_the_end base is_surnames s)
            end;
            Output.printf conf " (%d)" cnt;
            Output.printf conf "</li>\n")
         (List.sort (fun (a, _) (b, _) -> compare_particle_at_the_end base is_surnames a b) l);
       Output.printf conf "</ul>\n";
       Output.printf conf "</li>\n")
    list;
  Output.printf conf "</ul>\n";
  Hutil.trailer conf

let print_alphabetic_small conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  Hutil.header conf title;
  if list = [] then ()
  else
    begin
      Output.printf conf "<ul>\n";
      List.iter
        (fun (_, s, cnt) ->
           Output.printf conf "<li>";
           Output.printf conf "<a href=\"%sm=%s&v=%s&t=A\">" (commd conf) mode
             (Mutil.encode s);
           Output.print_string conf (particle_at_the_end base is_surnames s);
           Output.printf conf "</a>";
           Output.printf conf " (%d)" cnt;
           Output.printf conf "</li>\n")
        (List.sort (fun (_, a, _) (_, b, _) ->
             compare_particle_at_the_end base is_surnames a b) list);
      Output.printf conf "</ul>\n"
    end;
  Hutil.trailer conf

let print_frequency_any conf base is_surnames list len =
  let title _ = print_title conf base is_surnames "" len in
  let mode = if is_surnames then "N" else "P" in
  let n = ref 0 in
  Hutil.header conf title;
  Output.printf conf "<ul>\n";
  List.iter
    (fun (cnt, l) ->
       if !n > default_max_cnt then ()
       else
         begin
           Output.printf conf "<li>\n";
           Output.printf conf "%d\n" cnt;
           begin
             Output.printf conf "<ul>\n";
             List.iter
               (fun s ->
                  Output.printf conf "<li>";
                  Output.printf conf "<a href=\"%sm=%s&v=%s\">" (commd conf) mode
                    (Mutil.encode (Name.lower s));
                  Output.print_string conf (particle_at_the_end base is_surnames s);
                  Output.printf conf "</a>";
                  incr n;
                  Output.printf conf "</li>\n")
               l;
             Output.printf conf "</ul>\n"
           end;
           Output.printf conf "</li>\n"
         end)
    list;
  Output.printf conf "</ul>\n";
  Hutil.trailer conf


let print_frequency conf base is_surnames =
  let () = load_strings_array base in
  let (list, len) = Alln.select_names conf base is_surnames "" max_int in
  let list = groupby_count list in
  print_frequency_any conf base is_surnames list len

let print_alphabetic conf base is_surnames =
  let ini =
    match p_getenv conf.env "k" with
    | Some k -> k
    | _ -> ""
  in
  if p_getenv conf.base_env "fast_alphabetic" = Some "yes" && ini = ""
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
        let list = groupby_ini (Utf8.length ini + 1) list in
        print_alphabetic_all conf base is_surnames ini list len
      else print_alphabetic_small conf base is_surnames ini list len
  end

(* short print *)

let print_alphabetic_short conf base is_surnames ini list len =
  let title _ = print_title conf base is_surnames ini len in
  let mode = if is_surnames then "N" else "P" in
  let need_ref = len >= 250 in
  Hutil.header conf title;
  if need_ref then
    begin
      Output.printf conf "<p>\n";
      List.iter
        (fun (ini_k, _) ->
           let ini = ini_k in
           Output.printf conf "<a href=\"#a%s\">" ini;
           Output.print_string conf (Mutil.tr '_' ' ' ini);
           Output.printf conf "</a>\n")
        list;
      Output.printf conf "</p>\n"
    end;
  List.iter
    (fun (ini_k, l) ->
       let ini = ini_k in
       Output.printf conf "<p>\n";
       Mutil.list_iter_first
         (fun first (s, cnt) ->
            let href =
              " href=\"" ^ commd conf ^ "m=" ^ mode ^ "&v=" ^ Mutil.encode s ^ "&t=A\""
            in
            let name =
              if first && need_ref then " id=\"a" ^ ini ^ "\"" else ""
            in
            if not first then Output.printf conf ",\n";
            if href <> "" || name <> "" then
              Output.printf conf "<a%s%s>" href name;
            Output.print_string conf (particle_at_the_end base is_surnames s);
            if href <> "" || name <> "" then Output.printf conf "</a>";
            Output.printf conf " (%d)" cnt)
         (List.sort (fun (a, _) (b, _) -> Gutil.alphabetic_order a b) l);
       Output.printf conf "\n";
       Output.printf conf "</p>\n")
    list;
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
    let list = groupby_ini (Utf8.length ini + 1) list in
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

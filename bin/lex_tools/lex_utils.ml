open Geneweb

(**/**) (* Utils. *)

let skip_to_next_message ic =
  let rec loop () =
    let line = input_line ic in
    if Mutil.start_with "    " 0 line then line else loop ()
  in loop ()
;;

let get_all_versions ic =
  let rec loop accu =
    let line = try input_line ic with End_of_file -> "" in
    if line = "" then accu
    else
      try
        let i = String.index line ':' in
        let lang = String.sub line 0 i in
        let transl = String.sub line (i + 1) (String.length line - i - 1) in
        loop ((lang, transl) :: accu)
      with Not_found -> accu
  in loop []
;;


(**/**) (* Missing or unused translation. *)

let sources = ref "lib" ;;
let templates = ref ("hd"  ^ Filename.dir_sep ^ "etc") ;;
let tpl_ext = ref ".txt" ;;

let get_ml_files repo =
  Util.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".ml")
;;

let get_tpl_files repo =
  Util.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x !tpl_ext)
;;

let get_setup_files repo =
  Util.ls_r [repo]
  |> List.filter (fun x -> Filename.check_suffix x ".htm")
;;

(* Récupère tous les identifiants de message de lexicon. *)
let get_lexicon_msg lexicon =
  let lex = ref [] in
  match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          lex := msg :: !lex
        done
      with End_of_file -> ());
      close_in ic;
      List.rev_map (fun w -> String.sub w 4 (String.length w - 4)) !lex
  | None -> !lex
;;

let cut_all_msg_src s =
  let list = ref [] in
  let i = ref 0 in
  let regexp = Str.regexp "transl" in
  try
    while true do
      i := Str.search_forward regexp s !i;
      let start = String.index_from s !i '"' in
      let stop =
        let rec loop k =
          let stop = String.index_from s k '"' in
          if s.[stop - 1] = '\\' then loop (stop + 1)
          else stop
        in
        loop (start + 1)
      in
      list := String.sub s (start + 1) (stop - start - 1) :: !list;
      i := stop + 1
    done;
    !list
  with Not_found -> !list
;;

let get_msg_src repo =
  let msg = ref [] in
  (* TODO the current setup misses translations with the string on the next line !! *)
  let regexp = Str.regexp "transl.* \"" in
  List.iter
    (fun src ->
      match try Some (open_in src) with Sys_error _ -> None with
      | Some ic ->
          (try
            while true do
              let line = input_line ic in
              let has_msg =
                try
                  ignore (Str.search_forward regexp line 0);
                  true
                with Not_found -> false
              in
              if has_msg then msg := line :: !msg
              else ()
            done
          with End_of_file -> ());
          close_in ic;
      | None -> ())
    (get_ml_files repo);
  List.fold_left
    (fun accu msg -> List.rev_append (cut_all_msg_src msg) accu)
    [] !msg
;;

let cut_all_msg s =
  let list = ref [] in
  let i = ref 0 in
  try
    while true do
      let start = String.index_from s !i '[' in
      let stop = String.index_from s (start + 1) ']' in
      let w =
        if s.[start + 1] = '*' then
          String.sub s (start + 2) (stop - start - 2)
        else
          String.sub s (start + 1) (stop - start - 1)
      in
      let w =
        try
          (* loop si msg contient ':' *)
          let i = String.index w ':' in
          if (i + 2) < String.length w && w.[i + 1] = ':' && w.[i + 2] = ':'
          then String.sub w 0 i
          else w
        with Not_found -> w
      in
      let multi_msg w =
        try
          let i = String.index w ':' in
          if (i + 1) < String.length w && w.[i + 1] = ':' then
            list := (String.sub w 0 i) :: (String.sub w (i+2) (String.length w - i - 2)) :: !list
          else list := w :: !list
        with Not_found -> list := w :: !list
      in
      let not_msg =
        List.exists
          (fun x -> Mutil.start_with x 0 w)
          ["type="; "value="; "name="; "id="]
      in
      if not_msg then ()
      else multi_msg w;
      i := stop + 1
    done;
    !list
  with Not_found -> !list
;;

let get_msg_tpl repo =
  let msg = ref [] in
  let regexp = Str.regexp "[*?[a-z]+\\.*]" in
  List.iter
    (fun tpl ->
      match try Some (open_in tpl) with Sys_error _ -> None with
      | Some ic ->
          (try
            while true do
              let line = input_line ic in
              let has_msg =
                try
                  ignore (Str.search_forward regexp line 0);
                  true
                with Not_found -> false
              in
              if has_msg then msg := line :: !msg
              else ()
            done
          with End_of_file -> ());
          close_in ic;
      | None -> ())
    (get_tpl_files repo);
  List.fold_left
    (fun accu msg -> List.rev_append (cut_all_msg msg) accu)
    [] !msg
;;

let msg_setup = ref []

let get_msg_setup repo =
  let add_key_to_entry entry tpl_name =
    let entry = ("aa", " " ^ tpl_name) :: entry in
    let entry = List.sort (fun (x, _) (y, _) -> compare x y) entry in
    let key = 
      match List.find_opt (fun (k, _) -> k = "en") entry with
      | Some ("en", s) -> s
      | Some (_, _) -> "no key"
      | None -> "no key"
    in
    (key, entry)
  in
  let add_line line acc =
    if String.length line < 4 then acc
    else
      match String.index_opt line ':' with
      | Some i when i = 2 && String.length line > 4 -> (String.sub line 0 2, String.sub line 3 (String.length line - 3)) :: acc
      | _ -> 
        match acc with
        | (ll, str) :: acc -> (ll, str ^ String.sub line 3 (String.length line - 3)) :: acc
        | [] -> [(String.sub line 0 2, String.sub line 3 (String.length line - 3))]
  in
  let one_more_entry ic tpl_name =
    let entry =
      let rec loop acc =
        let line = input_line ic in
        match String.index_opt line ']' with
        | Some i -> acc
        | None -> loop (add_line line acc)
      in loop []
    in
    let entry = add_key_to_entry entry tpl_name in
    msg_setup := entry :: !msg_setup
  in
  List.iter
    (fun tpl ->
      let tpl_name = Filename.basename tpl in
      match try Some (open_in tpl) with Sys_error _ -> None with
      | Some ic ->
          (try
            while true do
              let line = input_line ic in
              let j = try String.index line ']' with Not_found -> 0 in
              match String.index_opt line '[' with
              | Some i when j = 0 -> one_more_entry ic tpl_name
              | _ -> ()
            done
          with End_of_file -> ());
          close_in ic;
      | None -> ())
    (get_setup_files repo);
    
;;

let change_msg_setup repo =
  let add_line line acc =
    if String.length line < 4 then acc
    else
      match String.index_opt line ':' with
      | Some i when i = 2 && String.length line > 4 -> (String.sub line 0 2, String.sub line 3 (String.length line - 3)) :: acc
      | _ -> 
        match acc with
        | (ll, str) :: acc -> (ll, str ^ String.sub line 3 (String.length line - 3)) :: acc
        | [] -> [(String.sub line 0 2, String.sub line 3 (String.length line - 3))]
  in
  let one_entry ic =
    let entry =
      let rec loop acc =
        let line = input_line ic in
        match String.index_opt line ']' with
        | Some i -> acc
        | None -> loop (add_line line acc)
      in loop []
    in entry
  in
  List.iter
    (fun tpl ->
      let tplo = (Filename.chop_suffix tpl ".htm") ^ ".new" in
      match (Some (open_in tpl), Some (open_out tplo)) with
      | (Some ic, Some oc) ->
          (try
            while true do
              let line = input_line ic in
              output oc (Bytes.of_string line) 0 (String.length line);
              let j = try String.index line ']' with Not_found -> 0 in
              match String.index_opt line '[' with
              | Some i when j = 0 ->
                  let entry = one_entry ic in
                  let str =
                    begin match List.find_opt (fun (k, _) -> k = "en") entry with
                    | Some ("en", str) -> str
                    | _ -> Printf.sprintf "Missing en entry in %s]\n" tplo
                    end
                  in
                  let str = if str.[0] = ' '
                    then String.sub str 1 (String.length str - 1)
                    else str
                  in
                  output oc (Bytes.of_string str) 0 (String.length str);
                  output oc (Bytes.of_string "]\n") 0 2;
                  if String.length str > 80 then
                  begin
                    let bytes = Bytes.of_string str in
                    output oc (Bytes.of_string "%(\n") 0 3;
                    let rec loop i =
                      let len = if i + 80 < Bytes.length bytes then 80
                      else (Bytes.length bytes - i) in
                      output oc bytes i len;output oc (Bytes.of_string "\n") 0 1;
                      if i + len < (Bytes.length bytes) then loop (i + 80) else ()
                    in loop 0;
                    output oc (Bytes.of_string "%)\n") 0 3
                  end
              | _ -> output oc (Bytes.of_string "\n") 0 1;
            done
          with End_of_file -> ());
          close_in ic;
          close_out oc;
      | (_, _) -> ())
    (get_setup_files repo);
;;

let add_key_to_msg_setup msg_setup =
  let msg_setup =
    let rec loop accu list = 
      match list with
      | [] -> accu
      | entry :: list ->
          let entry = List.sort (fun (x, _) (y, _) -> compare x y) entry in
          match List.find_opt (fun (k, _) -> k = "en") entry with
          | Some ("en", s) -> loop ((s, entry) :: accu) list
          | Some (_, _) -> loop accu list
          | None -> loop accu list
      
    in loop [] msg_setup
  in
  msg_setup
;;

module StringSet = Set.Make
  (struct
    type t = string
    let compare = Stdlib.compare
   end)
;;

let sort_uniq cmp l =
  let list =
    List.fold_left
      (fun accu e -> StringSet.add e accu)
      StringSet.empty l
  in
  List.sort cmp (StringSet.elements list)
;;

(* Essaie de chercher tous les identifiants de message du répository et *)
(* recherche s'il ne sont plus utilisés pour au contraire non trdauit.  *)
let missing_or_unused_msg lexicon repo log =
  let lexicon =
    if Filename.is_relative lexicon then
      Filename.concat (Sys.getcwd ()) lexicon
    else lexicon
  in
  let repo =
    if Filename.is_relative repo then
      Filename.concat (Sys.getcwd ()) repo
    else repo
  in
  let repo_src = Filename.concat repo !sources in
  let repo_tpl = Filename.concat repo !templates in

  let lex = get_lexicon_msg lexicon in
  let msg_src = get_msg_src repo_src in
  let msg_tpl = get_msg_tpl repo_tpl in
  let msg =
    sort_uniq
      (fun x y ->
        Stdlib.compare
          (String.lowercase_ascii x) (String.lowercase_ascii y))
      (List.rev_append msg_src msg_tpl)
  in

  if log then begin
    (match try Some (open_out "log_lex") with Sys_error _ -> None with
    | Some oc ->
        List.iter (fun w -> Printf.fprintf oc "%s\n" w) lex;
        close_out oc
    | None -> ());
    (match try Some (open_out "log_msg") with Sys_error _ -> None with
    | Some oc ->
        List.iter (fun w -> Printf.fprintf oc "%s\n" w) msg;
        close_out oc
    | None -> ());
    print_endline
      "View log_lex for lexicon msg and log_msg for src and tpl msg."
  end
  else begin
    Printf.fprintf stdout
      "\nMessage not used anymore in %s and %s :\n" repo_src repo_tpl;
    flush stdout;
    List.iter
      (fun w ->
        if List.mem w msg then ()
        else print_endline w)
      lex;

    Printf.fprintf stdout
      "\nMessage from %s and %s not in lexicon :\n" repo_src repo_tpl;
    flush stdout;
    List.iter
      (fun w ->
        if List.mem w lex then ()
        else print_endline w)
      msg
  end
;;


(**/**) (* Missing translation. *)

let lang_gw =
  [ "af"; "bg"; "br"; "ca"; "cs"; "da"; "de"; "en"; "eo"; "es"; "et"; "fi";
    "fr"; "he"; "is"; "it"; "lv"; "nl"; "no"; "pl"; "pt"; "pt-br"; "ro"; "ru";
    "sl"; "sv"; "zh" ]
;;

let lang_gnt = [ "de"; "en"; "es"; "fi"; "fr"; "it"; "nl"; "no"; "sv" ] ;;

let lang_cust = ref [] ;;
let lang_setup = ref [] ;;

let missing_languages list languages =
  List.fold_left
    (fun accu lang ->
       if not (List.mem_assoc lang list) then (lang :: accu)
       else accu)
    [] languages
;;

let print_transl_en_fr list =
  let en_transl = try List.assoc "en" list with Not_found -> "" in
  let fr_transl = try List.assoc "fr" list with Not_found -> "" in
  if en_transl <> "" then print_endline ("en:" ^ en_transl);
  if fr_transl <> "" then print_endline ("fr:" ^ fr_transl)
;;

let sort_setup_translations msg_setup =
  let list = List.sort_uniq (fun (x, _) (y, _) -> compare x y) msg_setup in
    List.iter
      (fun (k, trl) ->
        print_endline ("\n   " ^ k);
        List.iter (fun (lg, tr) -> print_endline (lg ^ ":" ^ tr)) trl
      ) list
;;

let missing_setup_translations msg_setup languages =
  if List.length languages <= 1 && List.length languages > 0 then
    let lg = List.hd languages in (* TODO just one language for the time being *)
    List.iter
      (fun (k, trl) -> 
        match List.find_opt (fun (k, _) -> k = lg) trl with
        | Some (_, _) -> ()
        | None -> begin
            print_endline ("\n   " ^ k);
            print_transl_en_fr trl; 
            print_endline (lg ^ ":")
          end
      )
    msg_setup
  else
    print_endline
      ("At least one -missing_one language, but only one: " ^
        (string_of_int (List.length languages)))
;;

let missing_translation lexicon languages =
  match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          let list = get_all_versions ic in
          let list' = missing_languages list languages in
          if list' <> [] then
            begin
              print_endline msg;
              print_transl_en_fr list;
              List.iter
                (fun lang -> print_endline (lang ^ ":")) (List.rev list');
              print_string "\n"
            end
        done
      with End_of_file -> ());
      close_in ic
  | None -> ()
;;

(**/**) (* Sorting. *)

module Lex_map = Map.Make
  (struct
    type t = string
    let compare x y =
      compare (String.lowercase_ascii x) (String.lowercase_ascii y)
   end)
;;

let merge = ref false ;;
let first = ref false ;;
let by_filename = ref false ;;

let sort_lexicon lexicon =
  let lex_sort = ref Lex_map.empty in
  (match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          let list = get_all_versions ic in
          let list' =
          List.sort (fun (x, _) (y, _) -> compare x y) list in
          let list' = if !merge then match Lex_map.find_opt msg !lex_sort with
            | Some list ->
                (* merge list and list' *)
                let list' =
                  let rec loop accu list =
                    match list with
                    | [] -> accu
                    | (k, v):: list -> loop ((k, v) :: accu) list
                  in loop list' list
                in
                if !first then
                  List.sort_uniq (fun (x, _) (y, _) -> compare x y) list'
                else
                  List.sort_uniq (fun (x, _) (y, _) -> compare x y) (List.rev list')
            | None -> list'
            else list'
          in
          lex_sort := Lex_map.add msg list' !lex_sort
        done
      with End_of_file -> ());
      close_in ic
  | None -> ());

  if !by_filename then
    let new_list =
      Lex_map.fold (fun m l acc -> l::acc) !lex_sort []
    in
    let new_list =
      List.sort (fun x y ->
        let (_, aax) = List.hd x in
        let (_, aay) = List.hd y in
        compare aax aay) new_list
    in
    List.iter (fun list ->
      begin
        let (_, msg) = List.find (fun (k, _) -> k = "en") list in
        print_endline ("   " ^ msg);
        List.iter (fun (lang, transl) -> print_endline (lang ^ ":" ^ transl)) list;
        print_string "\n"
      end) new_list
  else 
    Lex_map.iter
      (fun msg list ->
         print_endline msg;
         List.iter
           (fun (lang, transl) -> print_endline (lang ^ ":" ^ transl)) list;
         print_string "\n")
      !lex_sort
;;


(**/**) (* Main. *)

let lexicon = ref "" ;;
let lex_sort = ref false ;;
let change = ref false ;;
let missing_gw = ref false ;;
let missing_gnt = ref false ;;
let missing_setup = ref false ;;
let sort_setup = ref false ;;
let repo = ref "" ;;
let log = ref false ;;

let speclist =
  [("-sort", Arg.Set lex_sort, ": sort the lexicon (both key and content).");
   ("-merge", Arg.Set merge,
    ": merge rather than replace new lexicon entries.");
   ("-first", Arg.Set first,
    ": if multiple language entries, select first occurence.");
   ("-change", Arg.Set change,
    ": update .htm files in setup/lang.");
   ("-missing_gw", Arg.Set missing_gw,
    ": print missing translation managed by gw.");
   ("-missing_gnt", Arg.Set missing_gnt,
    ": print missing translation managed by gnt.");
   ("-missing_one", Arg.String (fun x -> lang_cust := x :: !lang_cust),
    ": print missing translation for these languages.");
   ("-missing_one_setup", Arg.String (fun x -> lang_setup := x :: !lang_setup),
    ": print missing translation in gwsetup for this language.");
   ("-sort_setup", Arg.Set sort_setup,
    ": sort translations in gwsetup.");
   ("-by_filename", Arg.Set by_filename,
    ": sort translations in gwsetup by filename (aa: file.htm).");
   ("-repo", Arg.String (fun x -> repo := x),
    ": check missing or unused key word.");
   ("-sources", Arg.String (fun x -> sources := x),
    ": where sources are (\"lib\", \"bin/setup\").");
   ("-templates", Arg.String (fun x -> templates := x),
    ": where templates are (\"hd/etc\", \"lang\").");
   ("-tpl_ext", Arg.String (fun x -> tpl_ext := x),
    ": templates extension (\".txt\", \".htm\").");
   ("-log", Arg.Set log,
    ": option for repo. Print in log files instead of stdout.")];
;;

let anonfun s = lexicon := s ;;
let usage = "Usage: lex_utils [options] lexicon" ;;

let main () =
  Arg.parse speclist anonfun usage;
  if !lexicon = "" && not (!change || !sort_setup || !lang_setup <> []) then (Arg.usage speclist usage; exit 2);
  let repo_setup = List.fold_left Filename.concat !repo ["bin"; "setup"] in
  get_msg_setup repo_setup;
  if !lex_sort then sort_lexicon !lexicon
  else if !missing_gw then missing_translation !lexicon lang_gw
  else if !missing_gnt then missing_translation !lexicon lang_gnt
  else if !sort_setup then sort_setup_translations !msg_setup
  else if !lang_cust <> [] then missing_translation !lexicon !lang_cust
  else if !lang_setup <> [] then missing_setup_translations !msg_setup !lang_setup
  else if !repo <> "" then missing_or_unused_msg !lexicon !repo !log
  else if !change then change_msg_setup repo_setup
  else ()
;;

Printexc.print main ()

(* Build : ocamlopt.opt lex_utils.ml -o lex_utils *)


(**/**) (* Utils. *)

let start_with s p =
  String.length p <= String.length s &&
    String.sub s 0 (String.length p) = p
;;

let skip_to_next_message ic =
  let rec loop () =
    let line = input_line ic in
    if start_with line "    " then line else loop ()
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

let get_files dir =
  (* Récupère tous les fichiers et dossier d'un dossier         *)
  (* et renvoie la liste des dossiers et la liste des fichiers. *)
  let read_files_folders fname =
    let list =
      List.map
        (fun file -> Filename.concat fname file)
        (Array.to_list (Sys.readdir fname))
    in
    List.partition Sys.is_directory list
  in
  (* Parcours récursif de tous les dossiers *)
  let rec loop l folders files =
    match l with
    | [] -> (folders, files)
    | x :: l ->
        let (fd, fi) = read_files_folders x in
        let l = List.rev_append l fd in
        let folders = List.rev_append fd folders in
        let files = List.rev_append fi files in
        loop l folders files
  in
  (* Toute l'arborescence de dir *)
  let (folders, files) = loop [dir] [] [] in
  (folders, files)
;;

let get_ml_files repo =
  let (folders, files) = get_files repo in
  List.filter (fun x -> Filename.check_suffix x ".ml") files
;;

let get_tpl_files repo =
  let (folders, files) = get_files repo in
  List.filter (fun x -> Filename.check_suffix x ".txt") files
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
  let regexp = Str.regexp "transl .* \"" in
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
          (fun x -> start_with w x)
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
  let regexp = Str.regexp "[*?[a-z]+]" in
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

module StringSet = Set.Make
  (struct
    type t = string
    let compare = Pervasives.compare
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
  let repo_src = Filename.concat repo "src" in
  let repo_tpl =
    List.fold_left Filename.concat repo ["hd"; "etc"]
  in

  let lex = get_lexicon_msg lexicon in
  let msg_src = get_msg_src repo in
  let msg_tpl = get_msg_tpl repo_tpl in
  let msg =
    sort_uniq
      (fun x y -> Pervasives.compare (String.lowercase x) (String.lowercase y))
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
    let compare x y = compare (String.lowercase x) (String.lowercase y)
   end)
;;

let sort_lexicon lexicon =
  let lex_sort = ref Lex_map.empty in
  (match try Some (open_in lexicon) with Sys_error _ -> None with
  | Some ic ->
      (try
        while true do
          let msg = skip_to_next_message ic in
          let list = get_all_versions ic in
          let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
          lex_sort := Lex_map.add msg list' !lex_sort
        done
      with End_of_file -> ());
      close_in ic
  | None -> ());
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
let missing_gw = ref false ;;
let missing_gnt = ref false ;;
let repo = ref "" ;;
let log = ref false ;;

let speclist =
  [("-sort", Arg.Set lex_sort, ": sort the lexicon (both key and content).");
   ("-missing_gw", Arg.Set missing_gw,
    ": print missing translation managed by gw.");
   ("-missing_gnt", Arg.Set missing_gnt,
    ": print missing translation managed by gnt.");
   ("-missing_one", Arg.String (fun x -> lang_cust := x :: !lang_cust),
    ": print missing translation for these languages.");
   ("-repo", Arg.String (fun x -> repo := x),
    ": check missing or unused key word.");
   ("-log", Arg.Set log,
    ": option for repo. Print in log files instead of stdout.")];
;;

let anonfun s = lexicon := s ;;
let usage = "Usage: lex_utils [options] lexicon" ;;

let main () =
  Arg.parse speclist anonfun usage;
  if !lexicon = "" then (Arg.usage speclist usage; exit 2);
  if !lex_sort then sort_lexicon !lexicon
  else if !missing_gw then missing_translation !lexicon lang_gw
  else if !missing_gnt then missing_translation !lexicon lang_gnt
  else if !lang_cust <> [] then missing_translation !lexicon !lang_cust
  else if !repo <> "" then missing_or_unused_msg !lexicon !repo !log
  else ()
;;

Printexc.catch main ()

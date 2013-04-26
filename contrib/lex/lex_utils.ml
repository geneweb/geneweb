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
      begin
        try
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
        with End_of_file -> ();
        close_in ic
      end
  | None -> let _ = print_endline "ko" in ()
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
      begin
        try
          while true do
            let msg = skip_to_next_message ic in
            let list = get_all_versions ic in
            let list' = List.sort (fun (x, _) (y, _) -> compare x y) list in
            lex_sort := Lex_map.add msg list' !lex_sort
          done
        with End_of_file -> ();
        close_in ic
      end
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

let speclist =
  [("-sort", Arg.Set lex_sort, ": sort the lexicon (both key and content)."); 
   ("-missing_gw", Arg.Set missing_gw, 
    ": print missing translation managed by gw.");
   ("-missing_gnt", Arg.Set missing_gnt, 
    ": print missing translation managed by gnt.");
   ("-missing_one", Arg.String (fun x -> lang_cust := x :: !lang_cust),
    ": print missing translation for these languages.")]
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
  else ()
;;

Printexc.catch main ()

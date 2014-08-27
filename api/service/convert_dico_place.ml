(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)

open Config
open Gwdb
open Def
open Util



let write_dico_place_set lang =
  begin
  let fname_csv = "dico_place_" ^ lang ^ ".csv" in
  let (towns, area_codes, countys, regions, countrys) =
    match
      try Some (Secure.open_in (Util.search_in_lang_path fname_csv))
      with Sys_error _ -> None
    with
    | Some ic ->
        begin
        let string_set_town = ref Api_search.StrSetAutoComplete.empty in
        let string_set_area_code = ref Api_search.StrSetAutoComplete.empty in
        let string_set_county = ref Api_search.StrSetAutoComplete.empty in
        let string_set_region = ref Api_search.StrSetAutoComplete.empty in
        let string_set_country = ref Api_search.StrSetAutoComplete.empty in
        (try
          while true do
            let line = input_line ic in
            match Api_util.explode line ',' with
            | [country; region; county; area_code; town] ->
                let split_place = line in
                if town <> "" then string_set_town := Api_search.StrSetAutoComplete.add split_place !string_set_town;
                let (_, split_place) = Api_util.split split_place ',' in
                if area_code <> "" then string_set_area_code := Api_search.StrSetAutoComplete.add split_place !string_set_area_code;
                let (_, split_place) = Api_util.split split_place ',' in
                if county <> "" then string_set_county := Api_search.StrSetAutoComplete.add split_place !string_set_county;
                let (_, split_place) = Api_util.split split_place ',' in
                if region <> "" then string_set_region := Api_search.StrSetAutoComplete.add split_place !string_set_region;
                let (_, split_place) = Api_util.split split_place ',' in
                if country <> "" then string_set_country := Api_search.StrSetAutoComplete.add split_place !string_set_country;
            | _ -> ()
          done;
        with End_of_file -> ());
        close_in ic;
        (List.sort Gutil.alphabetic_order (Api_search.StrSetAutoComplete.elements !string_set_town),
         List.sort Gutil.alphabetic_order (Api_search.StrSetAutoComplete.elements !string_set_area_code),
         List.sort Gutil.alphabetic_order (Api_search.StrSetAutoComplete.elements !string_set_county),
         List.sort Gutil.alphabetic_order (Api_search.StrSetAutoComplete.elements !string_set_region),
         List.sort Gutil.alphabetic_order (Api_search.StrSetAutoComplete.elements !string_set_country))
        end
    | None -> ([], [], [], [], [])
  in

  let _ =
  let fname_set = "dico_place_town_" ^ lang ^ ".list" in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname_set)
    with Sys_error _ -> None
  with
  | Some oc -> output_value oc (towns : (string list)); close_out oc
  | None -> ()
  in

  let _ =
  let fname_set = "dico_place_area_code_" ^ lang ^ ".list" in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname_set)
    with Sys_error _ -> None
  with
  | Some oc -> output_value oc (area_codes : (string list)); close_out oc
  | None -> ()
  in

  let _ =
  let fname_set = "dico_place_county_" ^ lang ^ ".list" in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname_set)
    with  Sys_error _ -> None
  with
  | Some oc -> output_value oc (countys : (string list)); close_out oc
  | None -> ()
  in

  let _ =
  let fname_set = "dico_place_region_" ^ lang ^ ".list" in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname_set)
    with  Sys_error _ -> None
  with
  | Some oc -> output_value oc (regions : (string list)); close_out oc
  | None -> ()
  in

  let _ =
  let fname_set = "dico_place_country_" ^ lang ^ ".list" in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname_set)
    with  Sys_error _ -> None
  with
  | Some oc -> output_value oc (countrys : (string list)); close_out oc
  | None -> ()
  in
  ()
  end
;;



let default_lang =
  ["de"; "en"; "es"; "fi"; "fr"; "it"; "nl"; "no"; "pt"; "sv"]
;;

let lang_list = ref [] ;;

let speclist =
   [ ("-add_lang", Arg.String (fun x -> lang_list := x :: !lang_list),
      "<lang>\n       Create cache file for this language") ]
;;
let anonfun _ = () ;;
let usage = "Usage: " ^ Sys.argv.(0) ;;

let main () =
  Arg.parse speclist anonfun usage;
  List.iter
    (fun lang -> write_dico_place_set lang)
    (default_lang @ !lang_list);
;;

main ();;

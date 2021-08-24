open Geneweb

module StrSet = Set.Make (String)

let rest str =
  let i = String.index str ',' in
  String.sub str (i + 1) (String.length str - i - 1)

let write_dico_place_set assets fname_csv lang =
  let (towns, area_codes, countys, regions, countrys) =
    let ic = open_in fname_csv in
    let string_set_town = ref StrSet.empty in
    let string_set_area_code = ref StrSet.empty in
    let string_set_county = ref StrSet.empty in
    let string_set_region = ref StrSet.empty in
    let string_set_country = ref StrSet.empty in
    let string_set r s = r := StrSet.add s !r in
    begin
      try while true do
          let line = input_line ic in
          match String.split_on_char ',' line with
          | [ town ; area_code ; county ; region ; country ] ->
            let place = line in
            if town <> "" then string_set string_set_town place ;
            let place = rest place in
            if area_code <> "" then string_set string_set_area_code place ;
            let place = rest place in
            if county <> "" then string_set string_set_county place ;
            let place = rest place in
            if region <> "" then string_set string_set_region place ;
            let place = rest place in
            if country <> "" then string_set string_set_country place ;
          | _ -> ()
        done
      with End_of_file -> close_in ic
    end ;
    let aux r =
      let a = StrSet.elements !r |> Array.of_list in
      Array.sort Gutil.alphabetic a ;
      a
    in
    ( aux string_set_town
    , aux string_set_area_code
    , aux string_set_county
    , aux string_set_region
    , aux string_set_country
    )
  in
  let generate k data =
    match Api_search.dico_fname assets lang k with
    | None -> ()
    | Some fname_set ->
      let ext_flags =
        [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
      in
      let oc = open_out_gen ext_flags 0o644 fname_set in
      output_value oc (data : Api_search.dico) ;
      close_out oc
  in
  generate `town towns ;
  generate `area_code area_codes ;
  generate `county countys ;
  generate `region regions ;
  generate `country countrys

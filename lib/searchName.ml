(* Copyright (c) 1998-2007 INRIA *)

let persons_of_stream conf base filter iperset stream max =
  let rec aux n iperset ipers =
    match Ext_seq.next ipers with
    | Some (_iper, _) when n <= 0 -> iperset
    | Some (iper, ipers) ->
        let p = Gwdb.poi base iper in
        if Person.is_visible conf base p && filter p then
          let iperset' = Gwdb.IperSet.add iper iperset in
          if iperset' == iperset then aux n iperset ipers
          else aux (n - 1) iperset' ipers
        else aux n iperset ipers
    | None -> iperset
  in
  if max <= 0 then None else Some (aux max iperset stream)

let n_persons_of_stream n conf base filter stream =
  let consume n iperset =
    match persons_of_stream conf base filter iperset stream n with
    | Some iperset -> Gwdb.IperSet.elements iperset
    | None -> Gwdb.IperSet.elements iperset
  in
  List.rev (consume n Gwdb.IperSet.empty)

let strip_particle base s =
  let particles = Gwdb.base_particles base in
  let p = Mutil.get_particle particles s in
  let len_particle = String.length p in
  String.sub s len_particle (String.length s - len_particle)

let start_with base pfx s =
  let s = Name.lower (strip_particle base s) in
  Ext_string.start_with pfx 0 s

type prefix = { kind : [ `First_name | `Surname ]; value : string }

let persons_of_prefixes_stream max conf base filter other_pfxs main_pfx =
  let main_stream =
    (match main_pfx.kind with
    | `First_name -> Gwdb.persons_stream_of_first_name_prefix
    | `Surname -> Gwdb.persons_stream_of_surname_prefix)
      base main_pfx.value
  in
  let other_map = Hashtbl.create 100 in
  let match_other_istr p other_pfx =
    let istr =
      (match other_pfx.kind with
      | `First_name -> Gwdb.get_first_name
      | `Surname -> Gwdb.get_surname)
        p
    in
    match Hashtbl.find_opt other_map (istr, other_pfx) with
    | Some value -> value
    | None ->
        let value =
          List.exists
            (start_with base (Name.lower (strip_particle base other_pfx.value)))
            (Name.split @@ Gwdb.sou base istr)
        in
        Hashtbl.add other_map (istr, other_pfx) value;
        value
  in
  let rec consume n results main_stream =
    match Ext_seq.next main_stream with
    | Some (iper, main_stream) ->
        if n = 0 then results
        else
          let p = Gwdb.poi base iper in
          if
            List.for_all (match_other_istr p) other_pfxs
            && Person.is_visible conf base p
            && filter p
          then
            let iperset' = Gwdb.IperSet.add iper results in
            if iperset' != results then consume (n - 1) iperset' main_stream
            else consume n results main_stream
          else consume n results main_stream
    | None -> results
  in
  Gwdb.IperSet.elements (consume max Gwdb.IperSet.empty main_stream)

let persons_starting_with ~conf ~base ~filter ~limit ~other_prefixes main_prefix
    =
  match other_prefixes with
  | _ :: _ ->
      persons_of_prefixes_stream limit conf base filter other_prefixes
        main_prefix
  | [] ->
      let stream =
        (match main_prefix.kind with
        | `First_name -> Gwdb.persons_stream_of_first_name_prefix
        | `Surname -> Gwdb.persons_stream_of_surname_prefix)
          base main_prefix.value
      in
      n_persons_of_stream limit conf base filter stream

let split_name ~kind name =
  List.filter_map
    (fun value ->
      Ext_option.return_if (Name.lower name <> "") (fun () -> { kind; value }))
    (Name.split name)

let sort_by_len l =
  let cmp pfx1 pfx2 = String.length pfx2.value - String.length pfx1.value in
  List.sort cmp l

let persons_starting_with ~conf ~base ~filter ~first_name_prefix ~surname_prefix
    ~limit =
  let l =
    let main_prefix, other_prefixes, partial_results =
      match
        ( sort_by_len (split_name ~kind:`First_name first_name_prefix),
          sort_by_len (split_name ~kind:`Surname surname_prefix) )
      with
      | [], [] -> (None, [], [])
      | main_prefix :: other_prefixes, [] ->
          ( Some main_prefix,
            other_prefixes,
            persons_starting_with ~conf ~base ~filter ~limit ~other_prefixes:[]
              { kind = `First_name; value = first_name_prefix } )
      | [], main_prefix :: other_prefixes ->
          ( Some main_prefix,
            other_prefixes,
            persons_starting_with ~conf ~base ~filter ~limit ~other_prefixes:[]
              { kind = `Surname; value = surname_prefix } )
      | (_ :: _ as first_name_prefixes), main_prefix :: other_prefixes ->
          ( Some main_prefix,
            first_name_prefixes @ other_prefixes,
            persons_starting_with ~conf ~base ~filter ~limit
              ~other_prefixes:
                [ { kind = `First_name; value = first_name_prefix } ]
              { kind = `Surname; value = surname_prefix } )
    in
    match main_prefix with
    | None -> []
    | Some _ when other_prefixes = [] -> partial_results
    | Some main_prefix ->
        let extra_results =
          let limit = limit - List.length partial_results in
          if limit = 0 then []
          else
            let filter =
              let partial_results = Gwdb.IperSet.of_list partial_results in
              fun person ->
                filter person
                && not (Gwdb.IperSet.mem (Gwdb.get_iper person) partial_results)
            in
            persons_starting_with ~conf ~base ~filter ~limit ~other_prefixes
              main_prefix
        in
        partial_results @ extra_results
  in
  let cmp_s proj p1 p2 =
    Utf8.compare (Gwdb.sou base (proj p1)) (Gwdb.sou base (proj p2))
  in
  List.sort
    (fun iper1 iper2 ->
      let p1 = Gwdb.poi base iper1 in
      let p2 = Gwdb.poi base iper2 in
      let sn = cmp_s Gwdb.get_surname p1 p2 in
      if sn <> 0 then sn else cmp_s Gwdb.get_first_name p1 p2)
    l

let empty_sn_or_fn base p =
  Gwdb.is_empty_string (Gwdb.get_surname p)
  || Gwdb.is_quest_string (Gwdb.get_surname p)
  || Gwdb.is_empty_string (Gwdb.get_first_name p)
  || Gwdb.is_quest_string (Gwdb.get_first_name p)
  || Name.lower (Gwdb.sou base (Gwdb.get_surname p)) = ""
  || Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  List.exists
    (fun n -> Name.strip n = k)
    (Gwdb.person_misc_names base p (Util.nobtit conf base))

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Gwdb.p_first_name base p) in
  let sn = Name.strip_lower (Gwdb.p_surname base p) in
  k = fn ^ sn && fn <> "" && sn <> ""

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      if person_is_approx_key base p k then p :: pl
      else if person_is_misc_name conf base p k then p :: pl
      else pl)
    pl []

(* search functions *)

let search_by_sosa ~conf ~base ~sosa =
  if Sosa.eq sosa Sosa.zero then None
  else
    Option.bind (Util.find_sosa_ref conf base) (fun sosa_ref ->
        Util.p_of_sosa conf base sosa sosa_ref)

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Person.is_visible conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
  | Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let list, _ =
        Search_name_display.persons_of_fsname conf base
          Gwdb.base_strings_of_surname
          (Gwdb.spi_find (Gwdb.persons_of_surname base))
          Gwdb.get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              let p = Util.pget conf base ip in
              if search_reject_p conf base p then pl
              else
                let fn1 =
                  Name.abbrev
                    (Name.lower (Gwdb.sou base (Gwdb.get_first_name p)))
                in
                if List.mem fn (Ext_string.split_on_char ' ' fn1) then p :: pl
                else pl)
            pl ipl)
        [] list
  | None -> []

let search_key_aux aux conf base an =
  let acc = Gutil.person_not_a_key_find_all base an in
  let an, acc =
    if acc = [] then
      match Util.name_with_roman_number an with
      | Some an1 ->
          let acc = Gutil.person_ht_find_all base an1 in
          if acc = [] then (an, []) else (an1, acc)
      | None -> (an, acc)
    else (an, acc)
  in
  let acc =
    List.filter_map
      (fun i ->
        let p = Util.pget conf base i in
        if search_reject_p conf base p then None else Some p)
      acc
  in
  let acc = aux conf base acc an in
  Gutil.sort_uniq_person_list base acc

let search_partial_key =
  search_key_aux (fun conf base acc an ->
      if acc = [] then search_by_name conf base an else acc)

let search_approx_key = search_key_aux select_approx_key

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  Option.bind (Gutil.person_of_string_key base an) (fun ip ->
      let p = Util.pget conf base ip in
      if search_reject_p conf base p then None else Some p)

let print_fiche conf base p =
  Util.record_visited conf (Gwdb.get_iper p);
  Perso.print conf base p

let search_sosa conf base s =
  match Sosa.of_string s with
  | Some sosa -> Option.to_list (search_by_sosa ~conf ~base ~sosa)
  | None -> []

let search_key conf base s = Option.to_list (search_by_key conf base s)

let print conf base specify unknown =
  let real_input label =
    match Util.p_getenv conf.Config.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  let bind s xs f =
    match xs with
    | [ p ] -> print_fiche conf base p
    | [] -> f ()
    | pl -> specify conf base s pl
  in
  match (real_input "p", real_input "n") with
  | Some fn, Some sn ->
      let s = fn ^ " " ^ sn in
      let ( >>= ) = bind s in
      search_key conf base s >>= fun () ->
      search_approx_key conf base s >>= fun () ->
      search_partial_key conf base s >>= fun () -> unknown conf s
  | Some fn, None ->
      let fres = Search_name_display.search_first_name conf base fn in
      if Search_name_display.fn_search_result_is_empty fres then unknown conf fn
      else Search_name_display.search_first_name_print conf base fres fn
  | None, Some sn ->
      let ( >>= ) = bind sn in
      search_sosa conf base sn >>= fun () ->
      search_key conf base sn >>= fun () ->
      let sres = Search_name_display.search_surname conf base sn in
      if not (Search_name_display.sn_search_result_is_empty sres) then
        Search_name_display.search_surname_print conf base unknown sres sn
      else
        let ( >>= ) = bind sn in
        search_approx_key conf base sn >>= fun () ->
        search_partial_key conf base sn >>= fun () -> unknown conf sn
  | None, None -> Hutil.incorrect_request conf

let search_by_sosa_in_env conf base =
  let ( >>= ) = Option.bind in
  Util.p_getenv conf.Config.env "surname" >>= fun str ->
  Sosa.of_string str >>= fun sosa -> search_by_sosa ~conf ~base ~sosa

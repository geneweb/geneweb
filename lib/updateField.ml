open Config
open Def
module Driver = Geneweb_db.Driver

type str_person = (Driver.iper, Update.key, string) gen_person
type str_family = (Update.key, Driver.ifam, string) gen_family

type result =
  | Modified of {
      label : string;
      before : string;
      after : string;
      after_html : string;
      raw : string;
      reload : string;
    }
  | Unchanged
  | Failed of string

type field = {
  lex : string;
  reload : bool;
  is_html : bool;
  display : config -> string -> string;
  get : str_person -> string option;
  set : str_person -> string -> str_person option;
}

let scalar ?(reload = false) ?(is_html = false) ?(display = fun _ v -> v) lex
    get set =
  {
    lex;
    reload;
    is_html;
    display;
    get = (fun p -> Some (get p));
    set = (fun p v -> Some (set p v));
  }

let raw_place conf v = Util.raw_string_of_place conf v

let set_pevent names set_evt set_legacy p v =
  let modified = ref false in
  let pevents =
    List.map
      (fun e ->
        if (not !modified) && List.mem e.epers_name names then (
          modified := true;
          set_evt e v)
        else e)
      p.pevents
  in
  { (set_legacy p v) with pevents }

let evt_place lex names get set_legacy =
  {
    lex;
    reload = false;
    is_html = false;
    display = raw_place;
    get = (fun p -> Some (get p));
    set =
      (fun p v ->
        Some
          (set_pevent names
             (fun e v -> { e with epers_place = v })
             set_legacy p v));
  }

let evt_src lex names get set_legacy =
  {
    lex;
    reload = false;
    is_html = true;
    display = (fun _ v -> v);
    get = (fun p -> Some (get p));
    set =
      (fun p v ->
        Some
          (set_pevent names
             (fun e v -> { e with epers_src = v })
             set_legacy p v));
  }

let fields =
  [
    ( "first_name",
      scalar ~reload:true "first name/first names"
        (fun p -> p.first_name)
        (fun p v -> { p with first_name = v }) );
    ( "surname",
      scalar ~reload:true "surname/surnames"
        (fun p -> p.surname)
        (fun p v -> { p with surname = v }) );
    ( "occ",
      {
        lex = "occ";
        reload = true;
        is_html = false;
        display = (fun _ v -> v);
        get = (fun p -> Some (string_of_int p.occ));
        set =
          (fun p v ->
            match int_of_string_opt v with
            | Some n when n >= 0 && n <= 99999 -> Some { p with occ = n }
            | _ -> None);
      } );
    ( "public_name",
      scalar "public name"
        (fun p -> p.public_name)
        (fun p v -> { p with public_name = v }) );
    ( "occu",
      scalar ~is_html:true "occupation/occupations"
        (fun p -> p.occupation)
        (fun p v -> { p with occupation = v }) );
    ( "birth_place",
      evt_place "birth place" [ Epers_Birth ]
        (fun p -> p.birth_place)
        (fun p v -> { p with birth_place = v }) );
    ( "baptism_place",
      evt_place "baptism place" [ Epers_Baptism ]
        (fun p -> p.baptism_place)
        (fun p v -> { p with baptism_place = v }) );
    ( "death_place",
      evt_place "death place" [ Epers_Death ]
        (fun p -> p.death_place)
        (fun p v -> { p with death_place = v }) );
    ( "burial_place",
      evt_place "burial place"
        [ Epers_Burial; Epers_Cremation ]
        (fun p -> p.burial_place)
        (fun p v -> { p with burial_place = v }) );
    ( "psources",
      scalar ~is_html:true "person source"
        (fun p -> p.psources)
        (fun p v -> { p with psources = v }) );
    ( "birth_src",
      evt_src "birth src" [ Epers_Birth ]
        (fun p -> p.birth_src)
        (fun p v -> { p with birth_src = v }) );
    ( "baptism_src",
      evt_src "baptism src" [ Epers_Baptism ]
        (fun p -> p.baptism_src)
        (fun p v -> { p with baptism_src = v }) );
    ( "death_src",
      evt_src "death src" [ Epers_Death ]
        (fun p -> p.death_src)
        (fun p v -> { p with death_src = v }) );
    ( "burial_src",
      evt_src "burial src"
        [ Epers_Burial; Epers_Cremation ]
        (fun p -> p.burial_src)
        (fun p v -> { p with burial_src = v }) );
  ]

let list_fields =
  [
    ( "qualifier",
      ( "qualifier",
        (fun (p : str_person) -> p.qualifiers),
        fun p l -> { p with qualifiers = l } ) );
    ("alias", ("alias", (fun p -> p.aliases), fun p l -> { p with aliases = l }));
    ( "first_name_alias",
      ( "first name alias",
        (fun p -> p.first_names_aliases),
        fun p l -> { p with first_names_aliases = l } ) );
    ( "surname_alias",
      ( "surname alias",
        (fun p -> p.surnames_aliases),
        fun p l -> { p with surnames_aliases = l } ) );
  ]

let split_indexed name =
  let n = String.length name in
  let rec start i =
    if i > 0 && name.[i - 1] >= '0' && name.[i - 1] <= '9' then start (i - 1)
    else i
  in
  let i = start n in
  if i = 0 || i = n then None
  else
    match int_of_string_opt (String.sub name i (n - i)) with
    | Some idx -> Some (String.sub name 0 i, idx)
    | None -> None

let resolve name =
  match List.assoc_opt name fields with
  | Some f -> Some f
  | None -> (
      match split_indexed name with
      | None -> None
      | Some (prefix, idx) -> (
          match List.assoc_opt prefix list_fields with
          | None -> None
          | Some (lex, get, set) ->
              Some
                {
                  lex;
                  reload = false;
                  is_html = false;
                  display = (fun _ v -> v);
                  get = (fun p -> List.nth_opt (get p) idx);
                  set =
                    (fun p v ->
                      let l = get p in
                      if idx < List.length l then
                        Some
                          (set p
                             (List.mapi (fun i x -> if i = idx then v else x) l))
                      else None);
                }))

let sent_field conf =
  List.find_map
    (fun (k, _) ->
      match resolve k with Some fld -> Some (k, fld) | None -> None)
    conf.env

let render_src conf base p s =
  let always_show_link =
    conf.wizard || (conf.friend && Driver.get_access p = SemiPublic)
  in
  Notes.wiki_of_source conf base ~always_show_link p s

let conflicting base ip sp =
  match Driver.person_of_key base sp.first_name sp.surname sp.occ with
  | Some ip' -> ip' <> ip
  | None -> false

let commit conf base o_p ip sp =
  let old_key =
    Util.make_key base (Driver.gen_person_of_person (Driver.poi base ip))
  in
  let p =
    UpdateIndOk.effective_mod ~skip_conflict:ip
      ~prerr:(fun _ _ err -> raise (Update.ModErr err))
      conf base sp
  in
  Driver.patch_person base p.key_index p;
  let new_key = Util.make_key base p in
  if old_key <> new_key then (
    let pgl =
      let db = Driver.read_nldb base in
      let db = Notes.merge_possible_aliases conf db in
      Notes.links_to_ind conf base db old_key None
    in
    Notes.update_notes_links_person base p;
    Notes.update_ind_key conf base pgl old_key new_key;
    Notes.update_cache_linked_pages conf Notes.Rename old_key new_key 0);
  Util.commit_patches conf base;
  let changed = U_Modify_person (o_p, Util.string_gen_person base p) in
  History.record conf base changed "mp";
  Update.delete_topological_sort_v conf base

let guard conf ~before ~v =
  let stale =
    match Util.p_getenv conf.env "prev" with
    | Some prev -> not (String.equal prev before)
    | None -> false
  in
  if stale then
    Some (Failed (Update.string_of_error conf Update.UERR_digest :> string))
  else if v = "" || String.equal before v then Some Unchanged
  else None

let perform conf base ip =
  let op = Driver.poi base ip in
  let o_p = Util.string_gen_person base (Driver.gen_person_of_person op) in
  let cur = UpdateInd.string_person_of base op in
  match sent_field conf with
  | None -> Unchanged
  | Some (name, fld) -> (
      let v =
        match Util.p_getenv conf.env name with
        | Some s -> Util.only_printable s
        | None -> ""
      in
      let v =
        match name with
        | "first_name" | "surname" ->
            if Name.contains_forbidden_char v then Name.purge v else v
        | _ -> v
      in
      let before = Option.value ~default:"" (fld.get cur) in
      match guard conf ~before ~v with
      | Some r -> r
      | None -> (
          match fld.set cur v with
          | None -> Unchanged
          | Some cur -> (
              let sp = UpdateIndOk.strip_person cur in
              if conflicting base ip sp then
                let p' =
                  Driver.poi base
                    (Option.get
                       (Driver.person_of_key base sp.first_name sp.surname
                          sp.occ))
                in
                Failed
                  (Update.string_of_error conf
                     (Update.UERR_already_defined (base, p', ""))
                    :> string)
              else
                match UpdateIndOk.check_person conf base sp with
                | Some err -> Failed (Update.string_of_error conf err :> string)
                | None -> (
                    try
                      commit conf base o_p ip sp;
                      let np = Driver.poi base ip in
                      let reload =
                        if fld.reload then
                          (Util.commd conf ^^^ Util.acces conf base np
                            :> string)
                        else ""
                      in
                      let label =
                        let s =
                          Utf8.capitalize_fst (Util.transl_nth conf fld.lex 0)
                        in
                        match split_indexed name with
                        | Some (_, idx) -> s ^ " " ^ string_of_int (idx + 1)
                        | None -> s
                      in
                      let after_html =
                        if fld.is_html then
                          let h = render_src conf base np v in
                          if String.equal name "occu" then Utf8.capitalize_fst h
                          else h
                        else ""
                      in
                      Modified
                        {
                          label;
                          before;
                          after = fld.display conf v;
                          after_html;
                          raw = v;
                          reload;
                        }
                    with Update.ModErr err ->
                      Failed (Update.string_of_error conf err :> string)))))

let set_marriage_field proj set_evt fam v =
  let derived l =
    let marr, _, _ = UpdateFamOk.reconstitute_from_fevents false "" l in
    proj marr
  in
  let n = List.length fam.fevents in
  let rec loop i =
    if i >= n then None
    else
      let l =
        List.mapi (fun j e -> if j = i then set_evt e v else e) fam.fevents
      in
      if String.equal (derived l) v then Some { fam with fevents = l }
      else loop (i + 1)
  in
  loop 0

type fam_field = {
  f_lex : config -> string;
  f_is_html : bool;
  f_get : str_family -> string;
  f_set : str_family -> string -> str_family option;
  f_display : config -> string -> string;
}

let fam_fields =
  [
    ( "marriage_place",
      {
        f_lex = (fun conf -> Util.transl_nth conf "marriage place" 0);
        f_is_html = false;
        f_get = (fun f -> f.marriage_place);
        f_set =
          (fun f v ->
            set_marriage_field
              (fun (_, _, place, _, _) -> place)
              (fun e v -> { e with efam_place = v })
              f v);
        f_display = (fun conf v -> Util.raw_string_of_place conf v);
      } );
    ( "marriage_src",
      {
        f_lex = (fun conf -> Util.transl_nth conf "marriage src" 0);
        f_is_html = true;
        f_get = (fun f -> f.marriage_src);
        f_set =
          (fun f v ->
            set_marriage_field
              (fun (_, _, _, _, src) -> src)
              (fun e v -> { e with efam_src = v })
              f v);
        f_display = (fun _ v -> v);
      } );
    ( "fsources",
      {
        f_lex = (fun conf -> Util.transl_nth conf "family/families" 0);
        f_is_html = true;
        f_get = (fun f -> f.fsources);
        f_set = (fun f v -> Some { f with fsources = v });
        f_display = (fun _ v -> v);
      } );
  ]

let sent_fam_field conf =
  List.find_map
    (fun (k, _) ->
      match List.assoc_opt k fam_fields with
      | Some fld -> Some (k, fld)
      | None -> None)
    conf.env

let commit_fam conf base ip sfam scpl sdes =
  let ifam = sfam.fam_index in
  let o_f =
    Util.string_gen_family base
      (Driver.gen_family_of_family (Driver.foi base ifam))
  in
  let _ifam, fam, cpl, des =
    UpdateFamOk.effective_mod conf base false sfam scpl sdes
  in
  UpdateFamOk.patch_parent_with_pevents base cpl;
  UpdateFamOk.patch_children_with_pevents base des;
  Notes.update_notes_links_family base fam;
  Util.commit_patches conf base;
  let p =
    Util.string_gen_person base
      (Driver.gen_person_of_person (Driver.poi base ip))
  in
  let n_f = Util.string_gen_family base fam in
  History.record conf base (U_Modify_family (p, o_f, n_f)) "mf";
  Update.delete_topological_sort conf base

let perform_fam conf base ip ifam =
  let sfam, scpl, sdes = UpdateFam.string_family_of base ifam in
  match sent_fam_field conf with
  | None -> Unchanged
  | Some (name, fld) -> (
      let v =
        match Util.p_getenv conf.env name with
        | Some s -> Util.only_printable s
        | None -> ""
      in
      let before = fld.f_get sfam in
      match guard conf ~before ~v with
      | Some r -> r
      | None -> (
          match fld.f_set sfam v with
          | None ->
              Failed
                (Update.string_of_error conf
                   (Update.UERR
                      (Adef.safe
                         (Utf8.capitalize_fst
                            (Util.transl conf "missing family event"))))
                  :> string)
          | Some sfam -> (
              try
                commit_fam conf base ip sfam scpl sdes;
                let label = Utf8.capitalize_fst (fld.f_lex conf) in
                let after_html =
                  if fld.f_is_html then
                    render_src conf base (Driver.poi base ip) v
                  else ""
                in
                Modified
                  {
                    label;
                    before;
                    after = fld.f_display conf v;
                    after_html;
                    raw = v;
                    reload = "";
                  }
              with Update.ModErr err ->
                Failed (Update.string_of_error conf err :> string))))

let respond conf result =
  let json =
    match result with
    | Modified r ->
        `Assoc
          [
            ("success", `Bool true);
            ("label", `String r.label);
            ("before", `String r.before);
            ("after", `String r.after);
            ("after_html", `String r.after_html);
            ("raw", `String r.raw);
            ("reload", `String r.reload);
          ]
    | Unchanged -> `Assoc [ ("success", `Bool false); ("message", `String "") ]
    | Failed message ->
        `Assoc [ ("success", `Bool false); ("message", `String message) ]
  in
  Output.header conf "Content-type: application/json";
  Output.header conf "Connection: close";
  Output.print_sstring conf (Yojson.Basic.to_string json);
  Output.flush conf

let print o_conf base =
  match Util.p_getenv o_conf.env "i" with
  | None -> Hutil.incorrect_request o_conf
  | Some i -> (
      match Driver.Iper.of_string i with
      | exception Failure _ -> Hutil.incorrect_request o_conf
      | ip -> (
          let conf = Update.update_conf o_conf in
          match Util.p_getenv conf.env "f" with
          | Some f -> (
              match Driver.Ifam.of_string f with
              | exception Failure _ -> Hutil.incorrect_request o_conf
              | ifam -> respond conf (perform_fam conf base ip ifam))
          | None -> respond conf (perform conf base ip)))

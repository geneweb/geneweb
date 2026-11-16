open Config
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let executable_stamp () =
  match Unix.stat Sys.executable_name with
  | st -> Printf.sprintf "%.0f/%d" st.Unix.st_mtime st.Unix.st_size
  | exception Unix.Unix_error _ -> ""

let mac conf kind payload =
  Digestif.SHA256.hmac_string
    ~key:(Option.value ~default:"" conf.secret_salt)
    (String.concat "\000"
       [ kind; Version.commit_id; executable_stamp (); payload ])

let seal conf kind v =
  let payload = Marshal.to_string v [] in
  Digestif.SHA256.to_hex (mac conf kind payload)
  ^ "."
  ^ Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet payload

let unseal conf kind s =
  match String.index_opt s '.' with
  | None -> None
  | Some i -> (
      let payload =
        Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet
          (String.sub s (i + 1) (String.length s - i - 1))
      in
      match (Digestif.SHA256.of_hex_opt (String.sub s 0 i), payload) with
      | Some m, Ok payload when Digestif.SHA256.equal m (mac conf kind payload)
        ->
          Some (Marshal.from_string payload 0)
      | Some _, Ok _ | Some _, Error _ | None, _ -> None)

let person_snapshot conf (sp : (Driver.iper, Update.key, string) gen_person) =
  seal conf "person" sp

let person_of_snapshot conf ip s =
  match
    (unseal conf "person" s
      : (Driver.iper, Update.key, string) gen_person option)
  with
  | Some sp when Driver.Iper.equal sp.key_index ip -> Some sp
  | Some _ | None -> None

let family_snapshot conf
    (fcd :
      (Update.key, Driver.ifam, string) gen_family
      * Update.key Adef.gen_couple
      * Update.key gen_descend) =
  seal conf "family" fcd

let family_of_snapshot conf ifam s =
  match
    (unseal conf "family" s
      : ((Update.key, Driver.ifam, string) gen_family
        * Update.key Adef.gen_couple
        * Update.key gen_descend)
        option)
  with
  | Some ((sfam, _, _) as fcd) when Driver.Ifam.equal sfam.fam_index ifam ->
      Some fcd
  | Some _ | None -> None

type conflict = {
  what : string;
  selector : string;
  base : string;
  mine : string;
  restore : (string * string) list;
  saved : (string * string) list;
  text : (string * string * string) option;
}

type change = { field : string; before : string; after : string }

type review = {
  mutable changes : change list;
  mutable conflicts : conflict list;
}

let note_change r field before after =
  r.changes <- { field; before; after } :: r.changes

let note_conflict r ?(selector = "") ?(restore = []) ?(saved = []) ?text what
    base mine =
  r.conflicts <-
    { what; selector; base; mine; restore; saved; text } :: r.conflicts

let merge r ?(selector = "") ?(restore = fun _ -> []) ~what ~text eq o a b =
  (match o with
  | Some o when not (eq o a) -> note_change r what (text o) (text a)
  | Some _ | None -> ());
  if eq a b then a
  else
    match o with
    | Some o when eq o a -> b
    | Some o when eq o b -> a
    | Some _ | None ->
        note_conflict r ~selector ~restore:(restore b) ~saved:(restore a) what
          (text a) (text b);
        a

let silent (o, a, b) =
  if a = b then a
  else match o with Some o when o = a -> b | Some _ | None -> a

let review_json ~kind known r =
  let str s = `String s in
  let pairs l = `List (List.map (fun (n, v) -> `List [ str n; str v ]) l) in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("kind", str kind);
         ("snapshot", `Bool known);
         ( "changes",
           `List
             (List.rev_map
                (fun c ->
                  `Assoc
                    [
                      ("what", str c.field);
                      ("before", str c.before);
                      ("after", str c.after);
                    ])
                r.changes) );
         ( "conflicts",
           `List
             (List.rev_map
                (fun c ->
                  `Assoc
                    [
                      ("what", str c.what);
                      ("selector", str c.selector);
                      ("base", str c.base);
                      ("mine", str c.mine);
                      ("restore", pairs c.restore);
                      ("saved", pairs c.saved);
                      ( "text",
                        match c.text with
                        | Some (name, saved, mine) ->
                            `Assoc
                              [
                                ("name", str name);
                                ("saved", str saved);
                                ("mine", str mine);
                              ]
                        | None -> `Null );
                    ])
                r.conflicts) );
       ])

let cap s = Utf8.capitalize_fst s
let label conf key n = cap (Util.transl_nth conf key n)
let by_name name = Printf.sprintf {|[name="%s"]|} name
let by_prefix prefix = Printf.sprintf {|[name^="%s"]|} prefix
let deleted conf = Util.transl_nth conf "edit conflict" 8

let date_text conf d =
  match Date.od_of_cdate d with
  | Some d -> (DateDisplay.string_of_date_aux ~link:false conf d :> string)
  | None -> ""

let key_id ((fn, sn, occ, _, _) : Update.key) = (fn, sn, occ)

let key_text ((fn, sn, occ, _, _) : Update.key) =
  if occ = 0 then fn ^ " " ^ sn else Printf.sprintf "%s.%d %s" fn occ sn

let eq_key k1 k2 = key_id k1 = key_id k2
let keys_text l = String.concat ", " (List.map key_text l)

let eq_witnesses w1 w2 =
  Array.length w1 = Array.length w2
  && Array.for_all2
       (fun (k1, kind1, note1) (k2, kind2, note2) ->
         eq_key k1 k2 && kind1 = kind2 && String.equal note1 note2)
       w1 w2

let witnesses_text w =
  keys_text (List.map (fun (k, _, _) -> k) (Array.to_list w))

let joined l = String.concat ", " (List.filter (fun s -> s <> "") l)

let pevent_name conf n =
  match EventForm.pevent_key n with
  | EventForm.Custom s -> s
  | EventForm.Key k when n = Epers_Occupation -> cap (Util.transl_nth conf k 0)
  | EventForm.Key k -> cap (Util.transl conf k)

let fevent_name conf n =
  match EventForm.fevent_key n with
  | EventForm.Custom s -> s
  | EventForm.Key k -> cap (Util.transl conf k)

let event_text conf name date place =
  joined [ name; date_text conf date; place ]

let align_events align norm l1 l2 =
  let tag l = List.map (fun e -> (norm e, e)) l in
  let t1 = tag l1 and t2 = tag l2 in
  List.map
    (fun (x, y) ->
      ( Option.map (fun x -> List.assq x t1) x,
        Option.map (fun y -> List.assq y t2) y ))
    (align (List.map fst t1) (List.map fst t2))

type 'e triple = { o : 'e option; a : 'e option; b : 'e option }

let event_triples ~align ~eq o al bl =
  match o with
  | None -> List.map (fun (a, b) -> { o = None; a; b }) (align al bl)
  | Some ol ->
      let oa = align ol al and ob = align ol bl in
      let partner x =
        Option.join
          (List.find_map
             (fun (y, a) ->
               match y with
               | Some y when y == x -> Some a
               | Some _ | None -> None)
             oa)
      in
      let added_by_b =
        List.filter_map (fun (y, b) -> if Option.is_none y then b else None) ob
      in
      let _, added_by_a =
        List.fold_left
          (fun (anchor, acc) (y, a) ->
            match (y, a) with
            | Some y, Some _ -> (Some y, acc)
            | None, Some a when not (List.exists (eq a) added_by_b) ->
                (anchor, (anchor, a) :: acc)
            | None, Some _ | Some _, None | None, None -> (anchor, acc))
          (None, []) oa
      in
      let after anchor acc =
        List.fold_left
          (fun acc (y, a) ->
            match (y, anchor) with
            | Some y, Some x when y == x ->
                { o = None; a = Some a; b = None } :: acc
            | None, None -> { o = None; a = Some a; b = None } :: acc
            | Some _, (Some _ | None) | None, Some _ -> acc)
          acc added_by_a
      in
      List.fold_right
        (fun (y, b) acc ->
          let t = { o = y; a = Option.bind y partner; b } in
          match y with Some y -> t :: after (Some y) acc | None -> t :: acc)
        ob []
      |> after None

let merge_events r ~deleted ~known ~name ~text ~eq ~fields ~selector triples =
  let rec loop k acc ts =
    match ts with
    | [] -> List.rev acc
    | t :: ts -> (
        let keep e = loop (k + 1) (e :: acc) ts in
        let drop () = loop k acc ts in
        match (t.o, t.a, t.b) with
        | o, Some a, Some b -> keep (fields k o a b)
        | Some o, Some a, None ->
            if eq o a then drop ()
            else (
              note_change r (name a) (text o) (text a);
              note_conflict r ~selector:(selector k) (name a) (text a) deleted;
              keep a)
        | Some o, None, Some b ->
            note_change r (name o) (text o) deleted;
            if eq o b then drop ()
            else (
              note_conflict r ~selector:(selector k) (name b) deleted (text b);
              keep b)
        | Some o, None, None ->
            note_change r (name o) (text o) deleted;
            drop ()
        | None, Some a, None ->
            if known then note_change r (name a) "" (text a)
            else note_conflict r ~selector:(selector k) (name a) (text a) "";
            keep a
        | None, None, Some b ->
            if not known then
              note_conflict r ~selector:(selector k) (name b) "" (text b);
            keep b
        | None, None, None -> drop ())
  in
  loop 1 [] triples

let event_fields conf r k name (date, place, reason, note, src, witnesses) =
  let what field =
    Printf.sprintf "%s %d · %s — %s" (label conf "event/events" 0) k name field
  in
  let text_field input key n (o, a, b) =
    let input = Printf.sprintf "%s%d" input k in
    merge r
      ~what:(what (Util.transl_nth conf key n))
      ~selector:(by_name input)
      ~restore:(fun v -> [ (input, v) ])
      ~text:Fun.id String.equal o a b
  in
  let date =
    let o, a, b = date in
    merge r
      ~what:(what (Util.transl_nth conf "date/dates" 0))
      ~selector:(by_prefix (Printf.sprintf "e_date%d_" k))
      ~text:(date_text conf) ( = ) o a b
  in
  let place = text_field "e_place" "place/places" 0 place in
  let reason = silent reason in
  let note = text_field "e_note" "note/notes" 0 note in
  let src = text_field "e_src" "source/sources" 0 src in
  let witnesses =
    let o, a, b = witnesses in
    merge r
      ~what:(what (Util.transl_nth conf "witness/witnesses" 1))
      ~selector:(by_prefix (Printf.sprintf "e%d_witn" k))
      ~text:witnesses_text eq_witnesses o a b
  in
  (date, place, reason, note, src, witnesses)

let norm_pevent e = Futil.map_pers_event key_id Fun.id e
let norm_fevent e = Futil.map_fam_event key_id Fun.id e

let merge_pevents conf r o a b =
  let eq x y = norm_pevent x = norm_pevent y in
  let name e = pevent_name conf e.epers_name in
  let fields k o a b =
    let t f = (Option.map f o, f a, f b) in
    let epers_date, epers_place, epers_reason, epers_note, epers_src, witnesses
        =
      event_fields conf r k (name a)
        ( t (fun e -> e.epers_date),
          t (fun e -> e.epers_place),
          t (fun e -> e.epers_reason),
          t (fun e -> e.epers_note),
          t (fun e -> e.epers_src),
          t (fun e -> e.epers_witnesses) )
    in
    {
      a with
      epers_date;
      epers_place;
      epers_reason;
      epers_note;
      epers_src;
      epers_witnesses = witnesses;
    }
  in
  merge_events r ~deleted:(deleted conf) ~known:(Option.is_some o) ~name
    ~text:(fun e -> event_text conf (name e) e.epers_date e.epers_place)
    ~eq ~fields
    ~selector:(Printf.sprintf "#pevent_select%d")
    (event_triples
       ~align:(align_events Event.align_pevents norm_pevent)
       ~eq o a b)

let merge_fevents conf r o a b =
  let eq x y = norm_fevent x = norm_fevent y in
  let name e = fevent_name conf e.efam_name in
  let fields k o a b =
    let t f = (Option.map f o, f a, f b) in
    let efam_date, efam_place, efam_reason, efam_note, efam_src, witnesses =
      event_fields conf r k (name a)
        ( t (fun e -> e.efam_date),
          t (fun e -> e.efam_place),
          t (fun e -> e.efam_reason),
          t (fun e -> e.efam_note),
          t (fun e -> e.efam_src),
          t (fun e -> e.efam_witnesses) )
    in
    {
      a with
      efam_date;
      efam_place;
      efam_reason;
      efam_note;
      efam_src;
      efam_witnesses = witnesses;
    }
  in
  merge_events r ~deleted:(deleted conf) ~known:(Option.is_some o) ~name
    ~text:(fun e -> event_text conf (name e) e.efam_date e.efam_place)
    ~eq ~fields
    ~selector:(Printf.sprintf "#fevent_select%d")
    (event_triples
       ~align:(align_events Event.align_fevents norm_fevent)
       ~eq o a b)

let death_kind d =
  match d with
  | NotDead -> ("NotDead", None)
  | Death (reason, _) -> ("Death", Some reason)
  | DeadYoung -> ("DeadYoung", None)
  | DeadDontKnowWhen -> ("Death", None)
  | DontKnowIfDead -> ("DontKnowIfDead", None)
  | OfCourseDead -> ("OfCourseDead", None)

let death_reason_code reason =
  match reason with
  | Killed -> "Killed"
  | Murdered -> "Murdered"
  | Executed -> "Executed"
  | Disappeared -> "Disappeared"
  | Unspecified -> "Unspecified"

let death_text conf d =
  match d with
  | NotDead -> Util.transl_nth conf "alive" 2
  | Death _ | DeadDontKnowWhen -> Util.transl_nth conf "died" 2
  | DeadYoung -> Util.transl_nth conf "died young" 2
  | DontKnowIfDead -> Util.transl_nth conf "don't know" 0
  | OfCourseDead -> Util.transl conf "of course dead"

let death_restore d =
  match death_kind d with
  | code, Some reason ->
      [ ("death", code); ("death_reason", death_reason_code reason) ]
  | code, None -> [ ("death", code) ]

let sex_code s = match s with Male -> "M" | Female -> "F" | Neuter -> "N"

let access_index a =
  match a with IfTitles -> 0 | Public -> 1 | SemiPublic -> 2 | Private -> 3

let access_code a =
  match a with
  | IfTitles -> "IfTitles"
  | Public -> "Public"
  | SemiPublic -> "SemiPublic"
  | Private -> "Private"

let titles_text l =
  String.concat "; "
    (List.map
       (fun t ->
         joined
           [
             (match t.t_name with Tmain -> "*" | Tname s -> s | Tnone -> "");
             t.t_ident;
             t.t_place;
           ])
       l)

let eq_relation r1 r2 =
  r1.r_type = r2.r_type
  && Option.equal eq_key r1.r_fath r2.r_fath
  && Option.equal eq_key r1.r_moth r2.r_moth
  && String.equal r1.r_sources r2.r_sources

let relations_text l =
  String.concat "; "
    (List.map
       (fun r -> keys_text (List.filter_map Fun.id [ r.r_fath; r.r_moth ]))
       l)

let merge_person conf ~snapshot a b =
  let o = person_of_snapshot conf a.key_index snapshot in
  let r = { changes = []; conflicts = [] } in
  let t f = (Option.map f o, f a, f b) in
  let text input what f =
    let o, a, b = t f in
    merge r ~what ~selector:(by_name input)
      ~restore:(fun v -> [ (input, v) ])
      ~text:Fun.id String.equal o a b
  in
  let strings prefix what f =
    let o, a, b = t f in
    merge r ~what ~selector:(by_prefix prefix) ~text:(String.concat ", ")
      (List.equal String.equal) o a b
  in
  let first_name =
    text "first_name" (label conf "first name/first names" 0) (fun p ->
        p.first_name)
  in
  let surname =
    text "surname" (label conf "surname/surnames" 0) (fun p -> p.surname)
  in
  let occ =
    let o, a, b = t (fun p -> p.occ) in
    merge r ~what:(label conf "number" 0) ~selector:(by_name "occ")
      ~restore:(fun v -> [ ("occ", string_of_int v) ])
      ~text:string_of_int Int.equal o a b
  in
  let public_name =
    text "public_name"
      (cap (Util.transl conf "public name"))
      (fun p -> p.public_name)
  in
  let qualifiers =
    strings "qualifier" (label conf "qualifier" 0) (fun p -> p.qualifiers)
  in
  let aliases = strings "alias" (label conf "alias" 0) (fun p -> p.aliases) in
  let first_names_aliases =
    strings "first_name_alias" (label conf "first name alias" 0) (fun p ->
        p.first_names_aliases)
  in
  let surnames_aliases =
    strings "surname_alias" (label conf "surname alias" 0) (fun p ->
        p.surnames_aliases)
  in
  let image = text "image" (label conf "image/images" 0) (fun p -> p.image) in
  let sex =
    let o, a, b = t (fun p -> p.sex) in
    merge r
      ~what:(cap (Util.transl conf "sex"))
      ~selector:(by_name "sex")
      ~restore:(fun s -> [ ("sex", sex_code s) ])
      ~text:(fun s ->
        Util.transl_nth conf "male/female/neuter" (Util.index_of_sex s))
      ( = ) o a b
  in
  let access =
    let o, a, b = t (fun p -> p.access) in
    merge r ~what:(label conf "access" 0) ~selector:(by_name "access")
      ~restore:(fun x -> [ ("access", access_code x) ])
      ~text:(fun x ->
        Util.transl_nth conf "iftitles/public/semipublic/private"
          (access_index x))
      ( = ) o a b
  in
  let occupation =
    text "occu" (label conf "occupation/occupations" 0) (fun p -> p.occupation)
  in
  let titles =
    let o, a, b = t (fun p -> p.titles) in
    merge r
      ~what:(label conf "title/titles" 1)
      ~selector:(by_prefix "t_") ~text:titles_text ( = ) o a b
  in
  let rparents =
    let o, a, b = t (fun p -> p.rparents) in
    merge r
      ~what:(label conf "relation/relations" 1)
      ~selector:(by_prefix "r") ~text:relations_text (List.equal eq_relation) o
      a b
  in
  let pevents =
    merge_pevents conf r (Option.map (fun p -> p.pevents) o) a.pevents b.pevents
  in
  let death =
    let o, a, b = t (fun p -> p.death) in
    merge r
      ~what:(pevent_name conf Epers_Death)
      ~selector:(by_name "death") ~restore:death_restore ~text:(death_text conf)
      (fun d1 d2 -> death_kind d1 = death_kind d2)
      o a b
  in
  let notes = text "notes" (label conf "note/notes" 1) (fun p -> p.notes) in
  let psources =
    text "src" (label conf "source/sources" 1) (fun p -> p.psources)
  in
  let p =
    {
      first_name;
      surname;
      occ;
      image;
      public_name;
      qualifiers;
      aliases;
      first_names_aliases;
      surnames_aliases;
      titles;
      rparents;
      related = a.related;
      occupation;
      sex;
      access;
      birth = silent (t (fun p -> p.birth));
      birth_place = silent (t (fun p -> p.birth_place));
      birth_note = silent (t (fun p -> p.birth_note));
      birth_src = silent (t (fun p -> p.birth_src));
      baptism = silent (t (fun p -> p.baptism));
      baptism_place = silent (t (fun p -> p.baptism_place));
      baptism_note = silent (t (fun p -> p.baptism_note));
      baptism_src = silent (t (fun p -> p.baptism_src));
      death;
      death_place = silent (t (fun p -> p.death_place));
      death_note = silent (t (fun p -> p.death_note));
      death_src = silent (t (fun p -> p.death_src));
      burial = silent (t (fun p -> p.burial));
      burial_place = silent (t (fun p -> p.burial_place));
      burial_note = silent (t (fun p -> p.burial_note));
      burial_src = silent (t (fun p -> p.burial_src));
      pevents;
      notes;
      psources;
      key_index = a.key_index;
    }
  in
  (p, review_json ~kind:"merge" (Option.is_some o) r)

let merge_children o a b =
  let mem x l = Array.exists (eq_key x) l in
  let kept =
    List.filter (fun x -> mem x b || not (mem x o)) (Array.to_list a)
  in
  let added =
    List.filter (fun x -> not (mem x o || mem x a)) (Array.to_list b)
  in
  Array.of_list (List.rev_append (List.rev kept) added)

let merge_family conf ~snapshot (a_fam, a_cpl, a_des) (b_fam, b_cpl, b_des) =
  let o = family_of_snapshot conf a_fam.fam_index snapshot in
  let r = { changes = []; conflicts = [] } in
  let t f = (Option.map (fun (fam, _, _) -> f fam) o, f a_fam, f b_fam) in
  let text input what f =
    let o, a, b = t f in
    merge r ~what ~selector:(by_name input)
      ~restore:(fun v -> [ (input, v) ])
      ~text:Fun.id String.equal o a b
  in
  let parent i get =
    merge r
      ~what:(label conf "father/mother" i)
      ~selector:(by_prefix (Printf.sprintf "pa%d_" (i + 1)))
      ~text:key_text eq_key
      (Option.map (fun (_, cpl, _) -> get cpl) o)
      (get a_cpl) (get b_cpl)
  in
  let father = parent 0 Adef.father in
  let mother = parent 1 Adef.mother in
  let fevents =
    merge_fevents conf r
      (Option.map (fun (fam, _, _) -> fam.fevents) o)
      a_fam.fevents b_fam.fevents
  in
  let children =
    let what = label conf "child/children" 1 in
    let text c = keys_text (Array.to_list c) in
    let eq c1 c2 =
      Array.length c1 = Array.length c2 && Array.for_all2 eq_key c1 c2
    in
    match o with
    | Some (_, _, o_des) ->
        if not (eq o_des.children a_des.children) then
          note_change r what (text o_des.children) (text a_des.children);
        merge_children o_des.children a_des.children b_des.children
    | None ->
        merge r ~what ~selector:(by_prefix "ch") ~text eq None a_des.children
          b_des.children
  in
  let comment =
    text "comment" (cap (Util.transl conf "comment")) (fun f -> f.comment)
  in
  let fsources =
    text "src" (label conf "source/sources" 1) (fun f -> f.fsources)
  in
  let fam =
    {
      marriage = silent (t (fun f -> f.marriage));
      marriage_place = silent (t (fun f -> f.marriage_place));
      marriage_note = silent (t (fun f -> f.marriage_note));
      marriage_src = silent (t (fun f -> f.marriage_src));
      witnesses = silent (t (fun f -> f.witnesses));
      relation = silent (t (fun f -> f.relation));
      divorce = silent (t (fun f -> f.divorce));
      fevents;
      comment;
      origin_file = silent (t (fun f -> f.origin_file));
      fsources;
      fam_index = a_fam.fam_index;
    }
  in
  ( (fam, Adef.couple father mother, { children }),
    review_json ~kind:"merge" (Option.is_some o) r )

let added_families conf base ip seen =
  let r = { changes = []; conflicts = [] } in
  let families = Driver.get_family (Driver.poi base ip) in
  Array.iteri
    (fun i ifam ->
      if i >= seen then
        let sp = Driver.poi base (Gutil.spouse ip (Driver.foi base ifam)) in
        note_change r
          (label conf "family/families" 0)
          ""
          (Util.gen_person_text ~html:false conf base sp :> string))
    families;
  review_json ~kind:"families" true r

let note_snapshot conf fname s = seal conf "note" (fname, s)

let note_of_snapshot conf fname s =
  match (unseal conf "note" s : (string * string) option) with
  | Some (f, s) when String.equal f fname -> Some s
  | Some _ | None -> None

let kept o x =
  let d_o, d_x = Difference.f o x in
  let m = Array.make (Array.length o) (-1) in
  let rec loop i j =
    if i < Array.length o then
      if d_o.(i) then loop (i + 1) j
      else if d_x.(j) then loop i (j + 1)
      else (
        m.(i) <- j;
        loop (i + 1) (j + 1))
  in
  loop 0 0;
  m

let merge_note conf o a b =
  let r = { changes = []; conflicts = [] } in
  let lines s = Array.of_list (String.split_on_char '\n' s) in
  let al = lines a and bl = lines b in
  let ol = match o with Some o -> lines o | None -> al in
  let ma = kept ol al and mb = kept ol bl in
  let sync =
    List.filter_map Fun.id
      (List.init (Array.length ol) (fun i ->
           if ma.(i) >= 0 && mb.(i) >= 0 then Some (i, ma.(i), mb.(i)) else None))
  in
  let sub l i j = Array.to_list (Array.sub l i (j - i)) in
  let text l = String.concat "\n" l in
  let where first count =
    if count <= 1 then
      Printf.sprintf "%s %d" (label conf "edit conflict" 18) first
    else
      Printf.sprintf "%s %d–%d"
        (label conf "edit conflict" 19)
        first
        (first + count - 1)
  in
  let rec loop (oi, ai, bi) acc n points =
    let (oj, aj, bj), rest =
      match points with
      | p :: rest -> (p, Some rest)
      | [] -> ((Array.length ol, Array.length al, Array.length bl), None)
    in
    let oc = sub ol oi oj and ac = sub al ai aj and bc = sub bl bi bj in
    let chunk =
      if ac = bc then ac
      else if Option.is_some o && oc = ac then bc
      else if Option.is_some o && oc = bc then ac
      else
        let around l =
          let l =
            match rest with
            | Some _ -> List.rev_append (List.rev l) [ al.(aj) ]
            | None -> l
          in
          text (match acc with line :: _ -> line :: l | [] -> l)
        in
        note_conflict r ~selector:{|[name="notes"]|}
          ~text:("notes", around ac, around bc)
          (where (n + 1) (List.length ac))
          (text ac) (text bc);
        ac
    in
    if Option.is_some o && oc <> ac then
      note_change r (where (n + 1) (List.length ac)) (text oc) (text ac);
    let acc = List.rev_append chunk acc and n = n + List.length chunk in
    match rest with
    | Some rest -> loop (oj + 1, aj + 1, bj + 1) (al.(aj) :: acc) (n + 1) rest
    | None ->
        (text (List.rev acc), review_json ~kind:"merge" (Option.is_some o) r)
  in
  loop (0, 0, 0) [] 0 sync

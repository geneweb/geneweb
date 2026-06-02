(* Always init with the same seed so we get reproductible random sequences. *)
let () = Random.init 0 in

let strings = ref [ "?"; "" ] in

let new_string =
  let icnt = ref 1 in
  fun () ->
    incr icnt;
    strings :=
      ({|>'>"><img src=x onerror=console.log(|} ^ string_of_int !icnt ^ {|)>|})
      :: !strings;
    !icnt
in

let new_string_str () =
  let i = new_string () in
  List.nth !strings i
in

let new_list gen n = List.init n (fun _ -> gen ()) in

let new_string_list = new_list new_string in

let new_date () = Date.cdate_of_date (Date.Dtext (new_string_str ())) in

let new_title () =
  Def.
    {
      t_name = Tname (new_string ());
      t_ident = new_string ();
      t_place = new_string ();
      t_date_start = new_date ();
      t_date_end = new_date ();
      t_nth = Random.int 10;
    }
in

(* FIXME: pevents *)
let person ?(rparents = []) ?(related = []) key_index =
  Def.
    {
      first_name = new_string ();
      surname = new_string ();
      occ = Random.int 10;
      image = new_string ();
      public_name = new_string ();
      qualifiers = new_string_list 5;
      aliases = new_string_list 5;
      first_names_aliases = new_string_list 5;
      surnames_aliases = new_string_list 5;
      titles = new_list new_title 5;
      rparents;
      related;
      occupation = new_string ();
      sex =
        (match Random.int 3 with
        | 0 -> Def.Male
        | 1 -> Def.Female
        | 2 -> Def.Neuter
        | _ -> assert false);
      access = Def.Public;
      birth = new_date ();
      birth_place = new_string ();
      birth_note = new_string ();
      birth_src = new_string ();
      baptism = new_date ();
      baptism_place = new_string ();
      baptism_note = new_string ();
      baptism_src = new_string ();
      death =
        Death
          ( (match Random.int 5 with
            | 0 -> Def.Killed
            | 1 -> Def.Murdered
            | 2 -> Def.Executed
            | 3 -> Def.Disappeared
            | 4 -> Def.Unspecified
            | _ -> assert false),
            new_date () );
      death_place = new_string ();
      death_note = new_string ();
      death_src = new_string ();
      burial =
        (match Random.int 2 with
        | 0 -> Def.Buried (new_date ())
        | 1 -> Def.Cremated (new_date ())
        | _ -> assert false);
      burial_place = new_string ();
      burial_note = new_string ();
      burial_src = new_string ();
      pevents = [];
      notes = new_string ();
      psources = new_string ();
      key_index;
    }
in

let ascend parents = { Gwdb.no_ascend with Def.parents } in

let union family = { Def.family } in

let family i = Def.{ (Mutil.empty_family 0) with fam_index = i } in

let couple a b = Adef.couple a b in

let descend children = { Def.children } in

let popule () =
  let bname = Sys.argv.(1) in
  Secure.set_base_dir (Filename.dirname bname);
  let child = person 0 in
  let father = person 1 in
  let mother = person 2 in
  let persons = [| child; father; mother |] in
  let ascends = [| ascend (Some 0); ascend None; ascend None |] in
  let unions = [| union [||]; union [| 0 |]; union [| 0 |] |] in
  let families = [| family 0 |] in
  let couples = [| couple 1 2 |] in
  let descends = [| descend [| 0 |] |] in
  let base_notes =
    Def.
      {
        nread = (fun _ _ -> new_string_str ());
        norigin_file = new_string_str ();
        efiles = (fun () -> []);
      }
  in
  let strings = Array.of_list (List.rev !strings) in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  ignore @@ Gwdb.make bname [] data
in

Printexc.print popule ()

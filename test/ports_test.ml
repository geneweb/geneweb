(** tests for import/export of .ged and .gw *)

(* TODO use https://gedcom.io/tools/#example-familysearch-gedcom-70-files to test gedcom v7.0 *)

open Alcotest

let ged_import filename base_name =
  let state = Ged2gwb_lib.State.make () in
  let state = { state with in_file = filename; out_file = base_name } in
  Ged2gwb_lib.make_base state

let ged_export base outfile =
  let opts = Gwexport.default_opts in
  let oc =
    let oc = open_out outfile in
    (outfile, output_string oc, fun () -> close_out oc)
  in
  let opts = { opts with base = Some (outfile, base); oc } in
  let select = Gwexport.select opts [] in
  Gwb2ged_lib.gwb2ged false opts select

let gw_export base outfile =
  let opts = Gwexport.default_opts in
  let oc = open_out outfile in
  let opts =
    {
      opts with
      base = Some (Gwdb.bname base, base);
      oc = (outfile, output_string oc, fun () -> close_out oc);
    }
  in
  Gwu_lib.gwu_simple ~export_isolated:true opts

let gw_import filename base_name =
  let state = Gwc_lib.State.default in
  (* set file to import *)
  let files = [ (filename, false, "merge", 0) ] in
  let state = { state with files; force = true; out_file = base_name } in
  Gwc_lib.make_base ~save_mem:false state;
  Gwdb.open_base base_name

(* -- tests on royal92.ged -- *)

let original_ged = Filename.concat "assets" "royal92.ged"
let exported_ged = Filename.concat "assets" "royal92_exported.ged"
let exported_gw = Filename.concat "assets" "royal92_exported.gw"
let base_name_1 = Filename.concat Filename.current_dir_name "royal_ged"
let base_name_2 = Filename.concat Filename.current_dir_name "royal_exported_ged"
let base_name_3 = Filename.concat Filename.current_dir_name "royal_exported_gw"

(* make all exported files and bases, compute person/fam numbers *)
let ( nb_real_persons_1,
      nb_families_1,
      nb_real_persons_2,
      nb_families_2,
      nb_real_persons_3,
      nb_families_3 ) =
  let base_1 = ged_import original_ged base_name_1 in
  let () = ged_export base_1 exported_ged in
  let nb_real_persons_1 = Gwdb.nb_of_real_persons base_1 in
  let nb_families_1 = Gwdb.nb_of_families base_1 in
  let () = Gwdb.close_base base_1 in
  (* export to .gw *)
  let base_2 = ged_import exported_ged base_name_2 in
  let nb_real_persons_2 = Gwdb.nb_of_real_persons base_2 in
  let nb_families_2 = Gwdb.nb_of_families base_2 in
  let () = gw_export base_2 exported_gw in
  let () = Gwdb.close_base base_2 in
  (* import gw *)
  let base_3 = gw_import exported_gw base_name_3 in
  let nb_real_persons_3 = Gwdb.nb_of_real_persons base_3 in
  let nb_families_3 = Gwdb.nb_of_families base_3 in
  let () = Gwdb.close_base base_3 in
  ( nb_real_persons_1,
    nb_families_1,
    nb_real_persons_2,
    nb_families_2,
    nb_real_persons_3,
    nb_families_3 )

let test_royal_ged () =
  (check int) "nb_real_persons_1 = 3010" 3010 nb_real_persons_1;
  (check int) "nb_families_1 = 1425" 1425 nb_families_1;
  (check int) "nb_real_persons_1 = nb_real_persons_2" nb_real_persons_1
    nb_real_persons_2;
  (check int) "nb_families_1 = nb_families_2" nb_families_1 nb_families_2;
  ()

let test_royal_gw () =
  (* TODO base_3 has 13 more persons
     is it related to https://github.com/geneweb/geneweb/issues/800 ? *)
  (check int) "nb_real_persons_1 = nb_real_persons_3" nb_real_persons_1
    nb_real_persons_3;
  (check int) "nb_families_1 = nb_families_3" nb_families_1 nb_families_3;
  ()

(* -- tests on abc.ged -- *)
(* tests on a base made from a ged import -> gw export -> gw import *)

(* -- get all data from abc base then close it -- *)
let base =
  let base =
    ged_import
      (Filename.concat "assets" "abc.ged")
      (Filename.concat Filename.current_dir_name "abc")
  in
  gw_export base "abc.gw";
  let abc_fname = Filename.concat Filename.current_dir_name "abc_gw" in
  gw_import "abc.gw" abc_fname

let a, b =
  let get_person fn sn =
    match Gwdb.person_of_key base fn sn 0 with
    | None -> failwith (Format.sprintf {|person "%s %s" not found|} fn sn)
    | Some iper -> Gwdb.poi base iper
  in
  (get_person "a" "A", get_person "b" "B")

let nb_of_persons = Gwdb.nb_of_persons base
let birth_cdate = Gwdb.get_birth a
let birth_note = Gwdb.sou base (Gwdb.get_birth_note a)
let birth_source = Gwdb.sou base (Gwdb.get_birth_src a)
let birth_place = Gwdb.sou base (Gwdb.get_birth_place a)
let fam = Gwdb.foi base (Gwdb.get_family a).(0)
let fevents = Gwdb.get_fevents fam
let pevents = Gwdb.get_pevents a

let marriage =
  match
    List.filter_map
      (fun event ->
        let event = Geneweb.Event.event_item_of_fevent ~sp:None event in
        if Geneweb.Event.get_name event = Fevent Def.Efam_Marriage then
          Some event
        else None)
      fevents
  with
  | [] -> failwith "no Efam_Marriage"
  | e :: [] -> e
  | _l -> failwith "duplicate Efam_Marriage"

let marriage_date = Geneweb.Event.get_date marriage

let marriage_witness, marriage_witness_kind, marriage_wnote =
  let witnesses = Geneweb.Event.get_witnesses_and_notes marriage in
  (*(check int) "Array witness length" 1 (Array.length witnesses);*)
  assert (Array.length witnesses = 1);
  let w, wk, wnote = witnesses.(0) in
  (Gwdb.poi base w, wk, Gwdb.sou base wnote)

let marriage_note = Gwdb.sou base (Geneweb.Event.get_note marriage)
let marriage_source = Gwdb.sou base (Geneweb.Event.get_src marriage)
let marriage_place = Gwdb.sou base (Geneweb.Event.get_place marriage)
let marriage_witness_fn = Gwdb.sou base (Gwdb.get_first_name marriage_witness)
let marriage_witness_sn = Gwdb.sou base (Gwdb.get_surname marriage_witness)

let diploma =
  match
    List.filter_map
      (fun event ->
        let event = Geneweb.Event.event_item_of_pevent event in
        if Geneweb.Event.get_name event = Pevent Def.Epers_Diploma then
          Some event
        else None)
      pevents
  with
  | [] -> failwith "no Epers_Diploma"
  | e :: [] -> e
  | _l -> failwith "duplicate Epers_Diploma"

let _w, diploma_witness_kind, diploma_wnote =
  let witnesses = Geneweb.Event.get_witnesses_and_notes diploma in
  (*(check int) "array witness len" 1 (Array.length witnesses);*)
  assert (Array.length witnesses = 1);
  let w, wk, wnote = witnesses.(0) in
  (w, wk, Gwdb.sou base wnote)

let diploma_note = Gwdb.sou base (Geneweb.Event.get_note diploma)
let diploma_source = Gwdb.sou base (Geneweb.Event.get_src diploma)
let title = List.hd (Gwdb.get_titles b)
let title_ident = Gwdb.sou base title.t_ident
let title_place = Gwdb.sou base title.t_place

let title_name_b =
  match title.t_name with
  | Tmain | Tnone -> false
  | Tname i -> Gwdb.sou base i = "This is a title name"

let fam_comment = Gwdb.sou base (Gwdb.get_comment fam)
let wiki = Gwdb.base_notes_read base ""
let () = Gwdb.close_base base

(* -- *)

let test_abc_number () =
  (check int) "nb_of_persons = 3" 3 nb_of_persons;
  ()

let test_abc_birth_event () =
  (match Date.cdate_to_dmy_opt birth_cdate with
  | None -> failwith {|no birth date for person "a A"|}
  | Some dmy ->
      assert (
        Date.compare_dmy dmy
          { day = 1; month = 1; year = 1990; prec = Date.Sure; delta = 0 }
        = 0));
  (check string) "birth note" "This is a note on a birth event" birth_note;
  (check string) "birth source" "This is a source on a birth event" birth_source;
  (check string) "birth place" "Paris" birth_place;
  ()

(* check family marriage event *)
let test_abc_marriage () =
  (match Date.cdate_to_dmy_opt marriage_date with
  | None -> failwith "no marriage date"
  | Some dmy ->
      (check bool) "compare dmy" true
        (Date.compare_dmy dmy
           { day = 18; month = 05; year = 2013; prec = Date.Sure; delta = 0 }
        = 0));
  (check string) "note on marriage event" "This is a note on a marriage event"
    marriage_note;
  (check string) "source on marriage event"
    "This is a source on a marriage event" marriage_source;
  (check string) "place marriage" "Lyon" marriage_place;
  (check string) "first_name_witness" "c" marriage_witness_fn;
  (check string) "surname_witness" "C" marriage_witness_sn;
  (check bool) "witness kind" true
    (marriage_witness_kind = Def.Witness_Attending);
  (check string) "marriage_wnote" "This is a witness note on a marriage event"
    marriage_wnote;
  ()

(* check Degree event *)
let test_abc_degree_event () =
  (check string) "note" "This is a note on a diploma event" diploma_note;
  (check string) "source" "This is a source on a diploma event" diploma_source;
  (check bool) "witness kind" true (diploma_witness_kind = Def.Witness);
  (check string) "wnote" "This is a witness note on a diploma event"
    diploma_wnote;
  ()

let test_abc_title () =
  (check string) "this is a title" "This is a title" title_ident;
  (check string) "this is a fief" "This is a Fief" title_place;
  (check bool) "title name" true title_name_b;
  (check int) "title nth" 1 title.t_nth

let test_abc_br () =
  (* TODO <br> bug at import https://github.com/geneweb/geneweb/issues/1002 *)
  (check string) "comment fam"
    "This is a comment on a family\n\nthis is a line after an empty line"
    fam_comment;
  ()

(* TODO do the same but for wizard notes *)
let test_abc_base_notes () =
  (* TODO gedcom import concatenate all wiki notes together in the main note.
     Instead it should recreate all the files and link them together by using
     the exported source page information *)
  (* empty string is for reading the main note,
     which is the starting point of the wiki *)
  let s =
    {|= Wiki notes! =

I '''love''' wikitext!

Here is a bullet list:
* ''first item''
* ''second item''
* ...

== Subsection ==

I also like [[[geneweb]]]
= GeneWeb =

'''GeneWeb''' est un logiciel de généalogie libre et gratuit doté d'une
interface web, utilisable aussi bien sur un ordinateur non connecté à Internet qu'en service web. Initialement conçu en 1997 par Daniel de Rauglaudre, il utilise des techniques de calcul de parenté et de consanguinité innovantes, mises au point par Daniel de Rauglaudre et Didier Rémy, directeur de recherche à l'Institut national de recherche en informatique et en automatique.|}
  in
  (check string) "wiki notes" s wiki;
  ()

let v =
  [
    ("gedcom-royal", [ test_case "Gedcom import export" `Quick test_royal_ged ]);
    ("gw-royal", [ test_case "Gw from gedcom" `Quick test_royal_gw ]);
    ( "abc",
      [
        test_case "Abc number" `Quick test_abc_number;
        test_case "Abc birth event" `Quick test_abc_birth_event;
        test_case "Abc marriage event" `Quick test_abc_marriage;
        test_case "Abc Degree event" `Quick test_abc_degree_event;
        test_case "Abc title" `Quick test_abc_title;
      ] );
    ( "note-br",
      [
        (* TODO <br> are inserted at gedcom import, removed at gedcom export, but not at gw export *)
        test_case "<br> in notes" `Quick test_abc_br;
        test_case "Abc base notes" `Quick test_abc_base_notes;
      ] );
  ]

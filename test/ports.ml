(* tests for import/export of .ged and .gw *)

let original_ged = Filename.concat "assets" "royal92.ged"
let exported_ged = Filename.concat "assets" "royal92_exported.ged"
let exported_gw = Filename.concat "assets" "royal92_exported.gw"

let ged_import filename =
  let state = Ged2gwb_lib.State.make () in
  let state = { state with in_file = filename } in
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

let gw_import filename =
  let base_filename = Filename.concat Filename.current_dir_name "royal_gw" in
  let state = Gwc_lib.State.default in
  (* set file to import *)
  let files = [ (filename, false, "merge", 0) ] in
  let state = { state with files; force = true; out_file = base_filename } in
  Gwc_lib.make_base ~save_mem:false state;
  Gwdb.open_base base_filename

(* import ged *)
let base_1 = ged_import original_ged
let nb_real_persons_1 = Gwdb.nb_of_real_persons base_1
let nb_families_1 = Gwdb.nb_of_families base_1

(* export ged *)
let () = ged_export base_1 exported_ged
let () = Gwdb.close_base base_1

(* re-import and check *)
let base_2 = ged_import exported_ged
let nb_real_persons_2 = Gwdb.nb_of_real_persons base_2
let nb_families_2 = Gwdb.nb_of_families base_2
let () = assert (nb_real_persons_1 = 3010)
let () = assert (nb_families_1 = 1425)
let () = assert (nb_real_persons_1 = nb_real_persons_2)
let () = assert (nb_families_1 = nb_families_2)

(* export to .gw *)
let () = gw_export base_2 exported_gw
let () = Gwdb.close_base base_2

(* import .gw and check *)
let base_3 = gw_import exported_gw
let nb_real_persons_3 = Gwdb.nb_of_real_persons base_3
let nb_families_3 = Gwdb.nb_of_families base_3

(* TODO base_3 has 13 more persons
   is it related to https://github.com/geneweb/geneweb/issues/800 ?
   let () = assert (nb_real_persons_1 = nb_real_persons_3)
*)
let () = assert (nb_families_1 = nb_families_3)
let () = Gwdb.close_base base_3

(* -- test abc.ged -- *)

(* test on a base made from a ged import -> gw export -> gw import *)
let base =
  let base = ged_import (Filename.concat "assets" "abc.ged") in
  gw_export base "abc.gw";
  gw_import "abc.gw"

let () = assert (Gwdb.nb_of_persons base = 3)

let get_person fn sn =
  match Gwdb.person_of_key base fn sn 0 with
  | None -> failwith (Format.sprintf {|person "%s %s" not found|} fn sn)
  | Some iper -> Gwdb.poi base iper

let a = get_person "a" "A"

(* check birth event *)
let () =
  (match Date.cdate_to_dmy_opt (Gwdb.get_birth a) with
  | None -> failwith {|no birth date for person "a A"|}
  | Some dmy ->
      assert (
        Date.compare_dmy dmy
          { day = 1; month = 1; year = 1990; prec = Def.Sure; delta = 0 }
        = 0));
  assert (
    Gwdb.sou base (Gwdb.get_birth_note a) = "This is a note on a birth event");
  assert (
    Gwdb.sou base (Gwdb.get_birth_src a) = "This is a source on a birth event");
  assert (Gwdb.sou base (Gwdb.get_birth_place a) = "Paris")

(* check family marriage event *)
let () =
  let fam = Gwdb.foi base (Gwdb.get_family a).(0) in
  assert (
    Gwdb.sou base (Gwdb.get_comment fam)
    = "This is a comment on a family\n\nthis is a line after an empty line");
  let fevents = Gwdb.get_fevents fam in
  let marriage =
    match
      List.filter (fun event -> event.Def.efam_name = Def.Efam_Marriage) fevents
    with
    | [] -> failwith "no Efam_Marriage"
    | e :: [] -> e
    | _l -> failwith "duplicate Efam_Marriage"
  in
  (match Date.cdate_to_dmy_opt marriage.efam_date with
  | None -> failwith "no marriage date"
  | Some dmy ->
      assert (
        Date.compare_dmy dmy
          { day = 18; month = 05; year = 2013; prec = Def.Sure; delta = 0 }
        = 0));

  assert (
    Gwdb.sou base marriage.efam_note = "This is a note on a marriage event");
  assert (
    Gwdb.sou base marriage.efam_src = "This is a source on a marriage event");
  assert (Gwdb.sou base marriage.efam_place = "Lyon");
  let witness, witness_kind, wnote =
    assert (Array.length marriage.efam_witnesses = 1);
    let w, wk, wnote = marriage.efam_witnesses.(0) in
    (Gwdb.poi base w, wk, Gwdb.sou base wnote)
  in
  assert (Gwdb.sou base (Gwdb.get_first_name witness) = "c");
  assert (Gwdb.sou base (Gwdb.get_surname witness) = "C");
  assert (witness_kind = Def.Witness_Attending);
  assert (wnote = "This is a witness note on a marriage event")

(* check Degree event *)
let () =
  let pevents = Gwdb.get_pevents a in
  let diploma =
    match
      List.filter
        (fun event -> event.Def.epers_name = Def.Epers_Diploma)
        pevents
    with
    | [] -> failwith "no Epers_Diploma"
    | e :: [] -> e
    | _l -> failwith "duplicate Epers_Diploma"
  in
  assert (Gwdb.sou base diploma.epers_note = "This is a note on a diploma event");
  assert (
    Gwdb.sou base diploma.epers_src = "This is a source on a diploma event");
  let _w, witness_kind, wnote =
    let witnesses = diploma.epers_witnesses in
    assert (Array.length witnesses = 1);
    let w, wk, wnote = witnesses.(0) in
    (w, wk, Gwdb.sou base wnote)
  in
  assert (witness_kind = Def.Witness);
  assert (
    wnote
    = "This is a witness note on a diploma event\n\
       -- this is the second line of the witness note --")

(* check title *)
let () =
  let b = get_person "b" "B" in
  match Gwdb.get_titles b with
  | [ title ] ->
      assert (Gwdb.sou base title.t_ident = "This is a title");
      assert (Gwdb.sou base title.t_place = "This is a Fief");
      assert (
        match title.t_name with
        | Tmain | Tnone -> false
        | Tname i -> Gwdb.sou base i = "This is a title name");
      assert (title.t_nth = 1)
  | _l -> assert false

(* -- test base notes (wiki) -- *)

(* TODO do the same but for wizard notes *)
let () =
  (* TODO gedcom import concatenate all wiki notes together in the main note.
     Instead it should recreate all the files and link them together by using
     the exported source page information *)
  (* empty string is for reading the main note,
     which is the starting point of the wiki *)
  let wiki = Gwdb.base_notes_read base "" in
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
  assert (wiki = s)

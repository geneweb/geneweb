(* PlaceRename and Driver.replace_string / find_string on a small base.

   Each scenario renames a term in places of its own, so they do not
   interfere:
   - A in place        "Coutances" in "Coutances, Manche, France"
   - B merge existing  "St-Lô" -> "Saint-Lô", which already exists
   - C merge in batch  "Paris" -> "Lutèce": two places become the same string
   - D new string      "Valognes", also used as a title domain
   - E conflict        "Tessy" -> "Tessy, Vire": the target is renamed too *)

open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver

let iper i = Driver.Iper.of_string (string_of_int i)
let ifam i = Driver.Ifam.of_string (string_of_int i)
let istr i = Driver.Istr.of_string (string_of_int i)
let istr_t = testable Driver.Istr.pp Driver.Istr.equal

let strings =
  [|
    "";
    "?";
    "Jean";
    "DUPONT";
    "comte";
    (* 5 *) "Coutances, Manche, France";
    (* 6 *) "St-L\195\180, Manche, France";
    (* 7 *) "Saint-L\195\180, Manche, France";
    (* 8 *) "Paris, Paris, France";
    (* 9 *) "Paris, Lut\195\168ce, France";
    (* 10 *) "Valognes, Manche, France";
    (* 11 *) "Tessy, Manche, France";
    (* 12 *) "Tessy, Vire, Manche, France";
    (* 13 *) "St Malo, Ille-et-Vilaine, France";
    (* 14 *) "Saint Malo, Ille-et-Vilaine, France";
    (* 15 *) "Saint-Malo, Ille-et-Vilaine, France";
  |]

(* person i is born at [births.(i)]; person 5 is also "comte de Valognes";
   persons 0 and 1 are married at St-Lô *)
let births = [| 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 |]

let with_base name k =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "%s_%d" name (Unix.getpid ()))
  in
  Mutil.rm_rf dir;
  Filesystem.create_dir ~parent:true dir;
  Secure.add_assets dir;
  Fun.protect ~finally:(fun () -> Mutil.rm_rf dir) @@ fun () ->
  let n = Array.length births in
  let persons =
    Array.mapi
      (fun i place ->
        let p =
          {
            (Mutil.empty_person 0 1) with
            key_index = i;
            first_name = 2;
            surname = 3;
            occ = i;
            birth_place = place;
          }
        in
        if i = 5 then
          {
            p with
            titles =
              [
                {
                  t_name = Tnone;
                  t_ident = 4;
                  t_place = 10;
                  t_date_start = Date.cdate_None;
                  t_date_end = Date.cdate_None;
                  t_nth = 0;
                };
              ];
          }
        else p)
      births
  in
  let data =
    ( ( persons,
        Array.make n { Driver.no_ascend with parents = None },
        Array.init n (fun i ->
            { family = (if i = 0 || i = 1 then [| 0 |] else [||]) }) ),
      ( [| { (Mutil.empty_family 0) with fam_index = 0; marriage_place = 6 } |],
        [| Adef.couple 0 1 |],
        [| { Def.children = [||] } |] ),
      Array.copy strings,
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  let bname = Filename.concat dir name in
  Driver.make bname [] data (k bname)

let birth base i = Driver.get_birth_place (Driver.poi base (iper i))

(* ------------------------------------------------------------------------ *)

let test_replace_string () =
  with_base "replace_string" @@ fun bname base ->
  let old_s = strings.(5) and new_s = "Coutance, Manche, France" in
  (check istr_t) "rewritten under its own id" (istr 5)
    (Driver.replace_string base old_s new_s);
  (check string) "new text" new_s (Driver.sou base (istr 5));
  (check (option istr_t))
    "new text found"
    (Some (istr 5))
    (Driver.find_string base new_s);
  (check (option istr_t)) "old text gone" None (Driver.find_string base old_s);
  (* the tombstone of the old text must not refuse a new insertion *)
  let reinserted = Driver.insert_string base old_s in
  (check bool) "old text inserted again, new id" false
    (Driver.Istr.equal reinserted (istr 5));
  (* the new string exists already: nothing changes, its id is returned *)
  (check istr_t) "existing target returned" (istr 7)
    (Driver.replace_string base strings.(6) strings.(7));
  (check string) "source unchanged" strings.(6) (Driver.sou base (istr 6));
  (check (option istr_t))
    "find without insert" None
    (Driver.find_string base "nowhere");
  Driver.commit_patches base;
  (* reloading the patches must not raise Conflict *)
  Driver.with_database bname @@ fun base ->
  (check string) "after reload" new_s (Driver.sou base (istr 5));
  (check (option istr_t))
    "index after reload"
    (Some (istr 5))
    (Driver.find_string base new_s);
  (check (option istr_t))
    "reinserted after reload" (Some reinserted)
    (Driver.find_string base old_s)

(* ------------------------------------------------------------------------ *)

let kind_s = function
  | PlaceRename.New_place -> "new place"
  | Merge_existing j -> "merge existing " ^ Driver.Istr.to_string j
  | Merge_batch -> "merge batch"

let plan conf base ?terms ~term ~new_term occs =
  let terms = Option.value ~default:[ term ] terms in
  PlaceRename.plan conf base ~terms ~new_term
    (List.map (fun (i, d) -> (istr i, d)) occs)

let kinds items =
  List.map
    (fun (it : PlaceRename.item) ->
      (Driver.Istr.to_string it.istr, kind_s it.kind))
    items

let result_t =
  testable
    (fun ppf (r : PlaceRename.result) ->
      Format.fprintf ppf
        "{places=%d; merged=%d; persons=%d; families=%d; complete=%b}" r.places
        r.merged r.persons r.families r.complete)
    ( = )

let res places merged persons families =
  { PlaceRename.places; merged; persons; families; complete = true }

let kinds_t = list (pair string string)

(* As m=MOD_DATA_OK: the records move to the id of the new string, the old
   string keeps its text. *)
let test_plan_apply () =
  with_base "plan_apply" @@ fun _ base ->
  let conf = Config.empty in
  let text i = Driver.sou base (birth base i) in
  (* A: a new place *)
  let items =
    plan conf base ~term:"Coutances" ~new_term:"Coutance" [ (5, Some 2) ]
  in
  (check kinds_t) "A kinds" [ ("5", "new place") ] (kinds items);
  (check result_t) "A result" (res 1 0 1 0) (PlaceRename.apply conf base items);
  (check string) "A person moved" "Coutance, Manche, France" (text 0);
  (check string) "A old string kept" strings.(5) (Driver.sou base (istr 5));
  (* B: the new string exists, references move to it *)
  let items =
    plan conf base ~term:"St-L\195\180" ~new_term:"Saint-L\195\180"
      [ (6, Some 2) ]
  in
  (check kinds_t) "B kinds" [ ("6", "merge existing 7") ] (kinds items);
  (check result_t) "B result" (res 1 1 1 1) (PlaceRename.apply conf base items);
  (check istr_t) "B person moved" (istr 7) (birth base 1);
  (check istr_t) "B family moved" (istr 7)
    (Driver.get_marriage_place (Driver.foi base (ifam 0)));
  (* C: both places become "Lutèce, Lutèce, France" *)
  let items =
    plan conf base ~term:"Paris" ~new_term:"Lut\195\168ce"
      [ (8, Some 1); (8, Some 2); (9, Some 2) ]
  in
  (check kinds_t) "C kinds"
    [ ("9", "new place"); ("8", "merge batch") ]
    (kinds items);
  (check result_t) "C result" (res 2 1 2 0) (PlaceRename.apply conf base items);
  (check string) "C text" "Lut\195\168ce, Lut\195\168ce, France" (text 3);
  (check istr_t) "C same id" (birth base 3) (birth base 4);
  (* D: "Valognes, ..." is also a title domain, which must not change *)
  let items =
    plan conf base ~term:"Valognes" ~new_term:"Valognes-Cotentin"
      [ (10, Some 2) ]
  in
  (check kinds_t) "D kinds" [ ("10", "new place") ] (kinds items);
  (check result_t) "D result" (res 1 0 1 0) (PlaceRename.apply conf base items);
  (check string) "D person place" "Valognes-Cotentin, Manche, France" (text 5);
  (check string) "D title domain unchanged" strings.(10)
    (Driver.sou base
       (List.hd (Driver.get_titles (Driver.poi base (iper 5)))).t_place);
  (* E: "Tessy, Manche" becomes "Tessy, Vire, Manche", renamed too: that
     string keeps its text, its own records move on *)
  let items =
    plan conf base ~term:"Tessy" ~new_term:"Tessy, Vire"
      [ (11, Some 2); (12, Some 3) ]
  in
  (check kinds_t) "E kinds"
    [ ("11", "merge existing 12"); ("12", "new place") ]
    (kinds items);
  (check result_t) "E result" (res 2 1 2 0) (PlaceRename.apply conf base items);
  (check string) "E Tessy, Manche" "Tessy, Vire, Manche, France" (text 6);
  (check string) "E Tessy, Vire" "Tessy, Vire, Vire, Manche, France" (text 7);
  (* F: a cluster of spellings, two of them merged into the third *)
  let items =
    plan conf base
      ~terms:[ "St Malo"; "Saint Malo" ]
      ~term:"" ~new_term:"Saint-Malo"
      [ (13, Some 2); (14, Some 2) ]
  in
  (check kinds_t) "F kinds"
    [ ("14", "merge existing 15"); ("13", "merge existing 15") ]
    (kinds items);
  (check result_t) "F result" (res 2 2 2 0) (PlaceRename.apply conf base items);
  (check istr_t) "F St Malo moved" (istr 15) (birth base 8);
  (check istr_t) "F Saint Malo moved" (istr 15) (birth base 9);
  (* nothing left to do: the person's place no longer has the term *)
  (check kinds_t) "A again" []
    (kinds
       (plan conf base ~term:"Coutances" ~new_term:"Coutance"
          [ (int_of_string (Driver.Istr.to_string (birth base 0)), Some 2) ]))

let v =
  [
    ( "place-rename-base",
      [
        test_case "replace_string / find_string" `Quick test_replace_string;
        test_case "plan and apply" `Quick test_plan_apply;
      ] );
  ]

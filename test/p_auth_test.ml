open Alcotest
open Def
open Geneweb
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

let date ?(prec = Adef.Sure) y =
  Date.cdate_of_od
    (Some
       (Adef.Dgreg
          ({ day = 0; month = 0; year = y; prec; delta = 0 }, Dgregorian)))

let died ?prec y = Death (Unspecified, date ?prec y)

(* A visitor, today 1 January 2026, with 100 years everywhere. *)
let visitor =
  Config.
    {
      empty with
      private_years = 100;
      private_years_death = 100;
      private_years_marriage = 100;
      today = { Adef.day = 1; month = 1; year = 2026; prec = Sure; delta = 0 };
    }

let pierre = 0
let marie = 1
let jean = 2
let paul = 3
let anne = 4
let luc = 5
let louis = 6
let rose = 7
let marc = 8
let henri = 9
let claire = 10
let eve = 11

(* fam 0: Pierre (died 1700) x Marie (undated): Jean (undated), Eve (born
   1990). fam 1: Paul x Anne (undated): Luc (born 1850). fam 2: Louis
   (undated) x Rose (died after 1700). fam 3: Henri (died 1950) x Claire
   (undated): Marc (undated). *)
let with_base k =
  let persons =
    [|
      (pierre, Date.cdate_None, died 1700);
      (marie, Date.cdate_None, DontKnowIfDead);
      (jean, Date.cdate_None, DontKnowIfDead);
      (paul, Date.cdate_None, DontKnowIfDead);
      (anne, Date.cdate_None, DontKnowIfDead);
      (luc, date 1850, DontKnowIfDead);
      (louis, Date.cdate_None, DontKnowIfDead);
      (rose, Date.cdate_None, died ~prec:After 1700);
      (marc, Date.cdate_None, DontKnowIfDead);
      (henri, Date.cdate_None, died 1950);
      (claire, Date.cdate_None, DontKnowIfDead);
      (eve, date 1990, DontKnowIfDead);
    |]
    |> Array.map (fun (occ, birth, death) ->
        {
          (Mutil.empty_person 0 1) with
          key_index = occ;
          first_name = 2;
          surname = 3;
          occ;
          birth;
          death;
        })
  in
  let spouses =
    [| (pierre, marie); (paul, anne); (louis, rose); (henri, claire) |]
  in
  let children = [| [| jean; eve |]; [| luc |]; [||]; [| marc |] |] in
  let nfam = Array.length spouses in
  let families = List.init nfam Fun.id in
  let n = Array.length persons in
  let data =
    ( ( persons,
        Array.init n (fun i ->
            {
              Driver.no_ascend with
              parents =
                List.find_opt (fun f -> Array.mem i children.(f)) families;
            }),
        Array.init n (fun i ->
            {
              family =
                Array.of_list
                  (List.filter
                     (fun f -> fst spouses.(f) = i || snd spouses.(f) = i)
                     families);
            }) ),
      ( Array.init nfam (fun i -> { (Mutil.empty_family 0) with fam_index = i }),
        Array.map (fun (f, m) -> Adef.couple f m) spouses,
        Array.map (fun children -> { Def.children }) children ),
      [| ""; "?"; "Jean"; "TEST" |],
      { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) } )
  in
  Driver.make "p_auth_base" [] data k

let iper i = Driver.Iper.of_string (string_of_int i)

let check_visible base msg i expected =
  (check bool) msg expected
    (GWPARAM.p_auth visitor base (Driver.poi base (iper i)))

let test_undated_from_relatives () =
  with_base @@ fun base ->
  let chk = check_visible base in
  chk "died more than private_years_death ago" pierre true;
  chk "spouse died long ago" marie true;
  chk "parent died long ago" jean true;
  chk "dated child" paul true;
  chk "dated child, other parent" anne true;
  chk "own birth" luc true;
  chk "a known birth is not replaced by the relatives" eve false

let test_after_proves_nothing () =
  with_base @@ fun base ->
  let chk = check_visible base in
  chk "died after a date" rose false;
  chk "spouse died after a date" louis false

let test_recent_relatives () =
  with_base @@ fun base ->
  let chk = check_visible base in
  chk "died recently" henri false;
  chk "spouse died recently" claire false;
  chk "parent died recently" marc false

let test_is_public_is_visitor_view () =
  with_base @@ fun base ->
  let wizard = { visitor with wizard = true; friend = true } in
  Collection.iter
    (fun p ->
      let i = string_of_int (Driver.Iper.hash (Driver.get_iper p)) in
      (check bool) ("is_public " ^ i)
        (GWPARAM.p_auth ~view:GWPARAM.As_visitor visitor base p)
        (Util.is_public wizard base p))
    (Driver.persons base)

let v =
  [
    ( "p-auth",
      [
        test_case "undated person, dated relatives" `Quick
          test_undated_from_relatives;
        test_case "an \"after\" date proves nothing" `Quick
          test_after_proves_nothing;
        test_case "recent relatives prove nothing" `Quick test_recent_relatives;
        test_case "is_public is the view of a visitor" `Quick
          test_is_public_is_visitor_view;
      ] );
  ]

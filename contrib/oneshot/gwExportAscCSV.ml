(* camlp4r *)
(* $Id: gwExportAscCSV.ml.ml,v 0.01 2014-04-16 10:05:38 flh Exp $ *)

open Def;
open Gwdb;
open Printf;


value is_printable =
  fun
  [ '\000'..'\031' -> False
  | _ -> True ]
;

value escape_quote s =
  let s =
    if Mutil.utf_8_db.val then s
    else Mutil.utf_8_of_iso_8859_1 s
  in
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else
      match s.[i] with
      [ '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
      | '"' ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store len s.[i])
      | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c) ]
;

(* TODO : tableau de mark pour les implexes. *)
value print_asc_csv base ip nb_gen =
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let p = poi base ip in
  let nb_person = 1 in
  loop 0 nb_person p where rec loop i nb_person p =
    if i = nb_gen then ()
    else do {
      (*
      let (birth, death, _) = Date.get_birth_death_date p in
      let birth =
        match birth with
        [ Some d -> Date.string_slash_of_date conf d
        | None -> "" ]
      in
      let death =
        match death with
        [ Some d -> Date.string_slash_of_date conf d
        | None -> "" ]
      in
      *)
      let (birth, death, _) = ("", "", "") in
      Printf.fprintf stdout "\"%d\";\"%s\";\"%s\";\"%s\";\"%s\";\n"
        nb_person
        (escape_quote (sou base (get_surname p)))
        (escape_quote (sou base (get_first_name p)))
        birth death;
      match get_parents p with
      [ Some ifam ->
          let cpl = foi base ifam in
          do {
            loop (succ i) (nb_person * 2) (poi base (get_father cpl));
            loop (succ i) (nb_person * 2 + 1) (poi base (get_mother cpl));
          }
      | None -> () ];
    }
;

value ind = ref 0;
value nb_gen = ref 4;
value bname = ref "";

value speclist =
  [("-i", Arg.Int (fun i -> ind.val := i), "ip of the person (default = 0)");
   ("-n", Arg.Int (fun i -> nb_gen.val := i),
    "number of generation (default = " ^ string_of_int nb_gen.val ^ ")")]
;
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " [-i #] [-n #] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let base = Gwdb.open_base bname.val in
    let ip = Adef.iper_of_int ind.val in
    print_asc_csv base ip nb_gen.val
  }
;

main ();











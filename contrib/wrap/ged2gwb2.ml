(* camlp5r pa_extend.cmo ../src/pa_lock.cmo *)
(* $Id: ged2gwb2.ml,v 1.00 2013-09-10 11:08:02 flh Exp $ *)
(* wrapper *)


value is_base_converted bname =
  let fname = Filename.concat bname "converted" in
  Sys.file_exists fname
(*
  let fname = "mon/nom/de/fichier" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop () where rec loop () =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line -> 
            if line = bname then do { close_in ic; True } else loop ()
        | None -> do { close_in ic; False } ]
  | None -> False ]
*)
;

value ged2gwb2_moins = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_moins/ged2gwb2";
value ged2gwb2_plus = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_plus/ged2gwb2";


(**/**) 


open Def;
open Printf;

value log_oc = ref stdout;
value in_file = ref "";

type month_number_dates =
  [ MonthDayDates
  | DayMonthDates
  | NoMonthNumberDates
  | MonthNumberHappened of string ]
;

type charset =
  [ Ansel
  | Ascii
  | Msdos
  | MacIntosh
  | Utf8 ]
;

type case =
  [ NoCase
  | LowerCase
  | UpperCase ]
;

value lowercase_first_names = ref False;
value case_surnames = ref NoCase;
value extract_first_names = ref False;
value extract_public_names = ref True;
value charset_option = ref None;
value charset = ref Ascii;
value alive_years = ref 80;
value dead_years = ref 120;
value try_negative_dates = ref False;
value no_negative_dates = ref False;
value month_number_dates = ref NoMonthNumberDates;
value no_public_if_titles = ref False;
value first_names_brackets = ref None;
value untreated_in_notes = ref False;
value force = ref False;
value default_source = ref "";
value default_name = ref "?";
value relation_status = ref Married;
value no_picture = ref False;


value rec lexing_date =
  parser
  [ [: `('0'..'9' as c); n = number (Buff.store 0 c) :] -> ("INT", n)
  | [: `('A'..'Z' as c); i = ident (Buff.store 0 c) :] -> ("ID", i)
  | [: `'('; len = text 0 :] -> ("TEXT", Buff.get len)
  | [: `'.' :] -> ("", ".")
  | [: `' ' | '\t' | '\013'; s :] -> lexing_date s
  | [: _ = Stream.empty :] -> ("EOI", "")
  | [: `x :] -> ("", String.make 1 x) ]
and number len =
  parser
  [ [: `('0'..'9' as c); a = number (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
and ident len =
  parser
  [ [: `('A'..'Z' as c); a = ident (Buff.store len c) :] -> a
  | [: :] -> Buff.get len ]
and text len =
  parser
  [ [: `')' :] -> len
  | [: `'('; len = text (Buff.store len '('); s :] ->
      text (Buff.store len ')') s
  | [: `c; s :] -> text (Buff.store len c) s
  | [: :] -> len ]
;

value set_undefined_death_interval s =
  try
    match Stream.of_string s with parser
    [ [: a = number 0; `'-'; b = number 0 :] ->
        do {
          eprintf "ay %s dy %s\n" a b;
          flush stderr;
          let a = if a = "" then alive_years.val else int_of_string a in
          let b =
            max a (if b = "" then dead_years.val else int_of_string b)
          in
          alive_years.val := a;
          dead_years.val := b;
          eprintf "ay %d dy %d\n" a b;
          flush stderr
        } ]
  with
  [ Stream.Error _ -> raise (Arg.Bad "bad parameter for -udi")
  | e -> raise e ]
;

value out_file = ref "a";
value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>\n       Output database (default: \"a\").");
   ("-f", Arg.Set force, "\n       Remove database if already existing");
   ("-log", Arg.String (fun s -> log_oc.val := open_out s),
    "<file>\n       Redirect log trace to this file.");
   ("-lf", Arg.Set lowercase_first_names, "   \
- Lowercase first names -
       Convert first names to lowercase letters, with initials in
       uppercase.");
   ("-ls", Arg.Unit (fun () -> case_surnames.val := LowerCase), "   \
- Lowercase surnames -
       Convert surnames to lowercase letters, with initials in
       uppercase. Try to keep lowercase particles.");
   ("-us", Arg.Unit (fun () -> case_surnames.val := UpperCase), "   \
- Uppercase surnames -
       Convert surnames to uppercase letters.");
   ("-fne",
    Arg.String
      (fun s ->
         if String.length s = 2 then
           first_names_brackets.val := Some (s.[0], s.[1])
         else
           raise
             (Arg.Bad
                "-fne option must be followed by a 2 characters string")),
    "\
be - First names enclosed -
       When creating a person, if the GEDCOM first name part holds
       a part between 'b' (any character) and 'e' (any character), it
       is considered to be the usual first name: e.g. -fne '\"\"' or
       -fne \"()\".");
   ("-efn", Arg.Set extract_first_names, "  \
- Extract first names -
       When creating a person, if the GEDCOM first name part holds several
       names, the first of this names becomes the person \"first name\" and
       the complete GEDCOM first name part a \"first name alias\".");
   ("-no_efn", Arg.Clear extract_first_names, "  \
- Dont extract first names - [default]
       Cancels the previous option.");
   ("-epn", Arg.Set extract_public_names, "  \
- Extract public names - [default]
       When creating a person, if the GEDCOM first name part looks like a
       public name, i.e. holds:
       * a number or a roman number, supposed to be a number of a
         nobility title,
       * one of the words: \"der\", \"den\", \"die\", \"el\", \"le\", \"la\",
         \"the\", supposed to be the beginning of a qualifier,
       then the GEDCOM first name part becomes the person \"public name\"
       and its first word his \"first name\".");
   ("-no_epn", Arg.Clear extract_public_names,
    "\n       Cancels the previous option.");
   ("-no_pit", Arg.Set no_public_if_titles, " \
- No public if titles -
       Do not consider persons having titles as public");
   ("-tnd", Arg.Set try_negative_dates, "  \
- Try negative dates -
       Set negative dates when inconsistency (e.g. birth after death)");
   ("-no_nd", Arg.Set no_negative_dates, " \
- No negative dates -
       Don't interpret a year preceded by a minus sign as a negative year");
   ("-nc", Arg.Clear Db2link.do_check, "\n       No consistency check");
   ("-nopicture", Arg.Set no_picture, " \
- Don't extract individual picture.");
   ("-udi", Arg.String set_undefined_death_interval, "\
x-y   - Undefined death interval -
       Set the interval for persons whose death part is undefined:
       - if before x years, they are considered as alive
       - if after y year, they are considered as death
       - between x and y year, they are considered as \"don't know\"
       Default x is " ^ string_of_int alive_years.val ^ " and y is " ^ string_of_int dead_years.val);
   ("-uin", Arg.Set untreated_in_notes,
    " - Untreated in notes -\n       Put untreated GEDCOM tags in notes");
   ("-ds", Arg.String (fun s -> default_source.val := s), " \
- Default source -
       Set the source field for persons and families without source data");
   ("-dn", Arg.String (fun s -> default_name.val := s), " \
- Default name -
       Set the first name or surname field for persons without name");
   ("-dates_dm", Arg.Unit (fun () -> month_number_dates.val := DayMonthDates),
    "\n       Interpret months-numbered dates as day/month/year");
   ("-dates_md", Arg.Unit (fun () -> month_number_dates.val := MonthDayDates),
    "\n       Interpret months-numbered dates as month/day/year");
   ("-rs_no_mention", Arg.Unit (fun () -> relation_status.val := NoMention),
    "\n       Force relation status to NoMention (default is Married)");
   ("-charset",
    Arg.String
      (fun
       [ "ANSEL" -> charset_option.val := Some Ansel
       | "ASCII" -> charset_option.val := Some Ascii
       | "MSDOS" -> charset_option.val := Some Msdos
       | _ -> raise (Arg.Bad "bad -charset value") ]),
    "\
[ANSEL|ASCII|MSDOS] - charset decoding -
       Force given charset decoding, overriding the possible setting in
       GEDCOM")]
;

value anonfun s =
  if in_file.val = "" then in_file.val := s
  else raise (Arg.Bad "Cannot treat several GEDCOM files")
;

value errmsg = "Usage: ged2gwb2 [<ged>] [options] where options are:";

value main () = do {
  Argl.parse speclist anonfun errmsg;
  let bname = out_file.val in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  if is_base_converted bname then 
    let () = Sys.argv.(0) := ged2gwb2_plus in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
  else 
    let () = Sys.argv.(0) := ged2gwb2_moins in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
};

main ();

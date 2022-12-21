open Ged2gwb_lib

let anonfun s =
  if !State.in_file = "" then State.in_file := s
  else raise (Arg.Bad "Cannot treat several GEDCOM files")

let errmsg = "Usage: ged2gwb [<ged>] [options] where options are:"
let out_file = ref "a"

let speclist =
  [
    ( "-o",
      Arg.String (fun s -> out_file := s),
      "<file> Output database (default: \"a\")." );
    ("-f", Arg.Set State.force, "Remove database if already existing");
    ( "-log",
      Arg.String (fun s -> State.log_oc := open_out s),
      "<file> Redirect log trace to this file." );
    ( "-lf",
      Arg.Set State.lowercase_first_names,
      "Convert first names to lowercase letters, with initials in uppercase." );
    ( "-trackid",
      Arg.Set State.track_ged2gw_id,
      "Print gedcom id to gw id matches." );
    ( "-ls",
      Arg.Unit (fun () -> State.case_surnames := LowerCase),
      "Convert surnames to lowercase letters, with initials in uppercase. Try \
       to keep lowercase particles." );
    ( "-us",
      Arg.Unit (fun () -> State.case_surnames := UpperCase),
      "Convert surnames to uppercase letters." );
    ( "-fne",
      Arg.String
        (fun s ->
          if String.length s = 2 then
            State.first_names_brackets := Some (s.[0], s.[1])
          else
            raise
              (Arg.Bad "-fne option must be followed by a 2 characters string")),
      "<be> When creating a person, if the GEDCOM first name part holds a part \
       between 'b' (any character) and 'e' (any character), it is considered \
       to be the usual first name: e.g. -fne '\"\"' or -fne \"()\"." );
    ( "-efn",
      Arg.Set State.extract_first_names,
      "When creating a person, if the GEDCOM first name part holds several \
       names, the first of this names becomes the person \"first name\" and \
       the complete GEDCOM first name part a \"first name alias\"." );
    ( "-no_efn",
      Arg.Clear State.extract_first_names,
      "Cancels the previous option." );
    ( "-epn",
      Arg.Set State.extract_public_names,
      "When creating a person, if the GEDCOM first name part looks like a \
       public name, i.e. holds:\n\
       * a number or a roman number, supposed to be a number of a nobility \
       title,\n\
       * one of the words: \"der\", \"den\", \"die\", \"el\", \"le\", \"la\", \
       \"the\", supposed to be the beginning of a qualifier, then the GEDCOM \
       first name part becomes the person \"public name\" and its first word \
       his \"first name\"." );
    ( "-no_epn",
      Arg.Clear State.extract_public_names,
      "Cancels the previous option." );
    ( "-no_pit",
      Arg.Set State.no_public_if_titles,
      "Do not consider persons having titles as public" );
    ( "-tnd",
      Arg.Set State.try_negative_dates,
      "Set negative dates when inconsistency (e.g. birth after death)" );
    ( "-no_nd",
      Arg.Set State.no_negative_dates,
      "Don't interpret a year preceded by a minus sign as a negative year" );
    ("-nc", Arg.Clear State.do_check, "No consistency check");
    ("-nopicture", Arg.Set State.no_picture, "Don't extract individual picture.");
    ( "-udi",
      Arg.String
        (fun s ->
          match String.index_opt s '-' with
          | Some i ->
              let a = String.sub s 0 i in
              let b = String.sub s (i + 1) (String.length s - i - 1) in
              let a = if a = "" then !State.alive_years else int_of_string a in
              let b =
                max a (if b = "" then !State.dead_years else int_of_string b)
              in
              State.alive_years := a;
              State.dead_years := b
          | None -> raise (Arg.Bad "bad parameter for -udi")),
      "x-y Set the interval for persons whose death part is undefined:\n\
       - if before x years, they are considered as alive\n\
       - if after y year, they are considered as death\n\
       - between x and y year, they are considered as \"don't know\"\n\
       Default x is "
      ^ string_of_int !State.alive_years
      ^ " and y is "
      ^ string_of_int !State.dead_years );
    ( "-uin",
      Arg.Set State.untreated_in_notes,
      "Put untreated GEDCOM tags in notes" );
    ( "-ds",
      Arg.Set_string State.default_source,
      "Set the source field for persons and families without source data" );
    ( "-dates_dm",
      Arg.Unit (fun () -> State.month_number_dates := DayMonthDates),
      "Interpret months-numbered dates as day/month/year" );
    ( "-dates_md",
      Arg.Unit (fun () -> State.month_number_dates := MonthDayDates),
      "Interpret months-numbered dates as month/day/year" );
    ( "-rs_no_mention",
      Arg.Unit (fun () -> State.relation_status := NoMention),
      "Force relation status to NoMention (default is Married)" );
    ( "-charset",
      Arg.String
        (function
        | "ANSEL" -> State.charset_option := Some Ansel
        | "ASCII" -> State.charset_option := Some Ascii
        | "MSDOS" -> State.charset_option := Some Msdos
        | _ -> raise (Arg.Bad "bad -charset value")),
      "[ANSEL|ASCII|MSDOS] Force given charset decoding, overriding the \
       possible setting in GEDCOM" );
    ( "-particles",
      Arg.String (fun s -> State.particles := Mutil.input_particles s),
      "<FILE> Use the given file as list of particles" );
  ]
  |> List.sort compare |> Arg.align

let main () =
  Arg.parse speclist anonfun errmsg;
  let state = State.make () in
  let base = make_base state in
  warning_month_number_dates ();
  if !State.do_check then (
    let base_error x =
      Geneweb.Check.print_base_error !State.log_oc base x;
      Printf.fprintf !State.log_oc "\n"
    in
    let base_warning = function
      | Def.UndefinedSex _ -> ()
      | x ->
          Geneweb.Check.print_base_warning !State.log_oc base x;
          Printf.fprintf !State.log_oc "\n"
    in
    Geneweb.Check.check_base base base_error base_warning ignore;
    flush !State.log_oc);
  if !State.log_oc != stdout then close_out !State.log_oc

let () =
  try main ()
  with e ->
    let e = match e with Ploc.Exc (_, e) -> e | _ -> e in
    Printf.fprintf !State.log_oc "Uncaught exception: %s\n"
      (Printexc.to_string e);
    if !State.log_oc != stdout then close_out !State.log_oc;
    exit 2

(* anoma.ml — GeneWeb anomalies analyzer.
   Reads a warning log produced when rebuilding a GeneWeb base, a per-base
   configuration file <bases_dir>/<basename>.cfg (WarningName=yes/no), and an
   "ignored" file of already-verified persons/families. Produces an HTML
   report with statistics, "extreme" statistics, and per-warning-type sorted
   unique lists of persons/families, each person linking to the base.

   Build:
     ocamlfind ocamlopt -package str -linkpkg gwwarn.ml -o gwwarn
   or (without ocamlfind):
     ocamlopt str.cmxa gwwarn.ml -o gwwarn
   or with dune, see the accompanying dune files.

   Usage:
     gwwarn <basename> -bd <bases_dir> -ok <ignored_file> -in <log_file>
            [-out <report.html>] [-url <base_url>] [-w | -nw]
*)

open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

(* ------------------------------------------------------------------ *)
(* Command line                                                        *)
(* ------------------------------------------------------------------ *)

let base_name = ref ""
let bases_dir = ref "."
let ignored_file = ref ""
let log_file = ref ""
let out_file = ref ""
let base_url = ref ""
let wizard = ref true

let usage =
  "usage: gwwarn <basename> -bd <bases_dir> -ok <ignored> -in <log_file> [-out \
   <report.html>] [-url <base_url>] [-w | -nw]"

let speclist =
  [
    ("-bd", Arg.Set_string bases_dir, "<dir>   bases directory (default .)");
    ("-ok", Arg.Set_string ignored_file, "<file>  ignored persons/families");
    ("-in", Arg.Set_string log_file, "<file>  warning log file");
    ( "-out",
      Arg.Set_string out_file,
      "<file>  output HTML (default <basename>_warnings.html)" );
    ( "-url",
      Arg.Set_string base_url,
      "<url>   base URL (default http://localhost:2317/<basename>)" );
    ( "-w",
      Arg.Set wizard,
      "        correction links use wizard access <basename>_w (default)" );
    ( "-nw",
      Arg.Clear wizard,
      "       correction links use plain access (for shared/public reports)" );
  ]

(* ------------------------------------------------------------------ *)
(* Basic helpers                                                       *)
(* ------------------------------------------------------------------ *)

let starts_with s pre =
  String.length s >= String.length pre
  && String.sub s 0 (String.length pre) = pre

let read_lines file =
  let ic = open_in file in
  let rec go acc =
    match input_line ic with
    | line -> go (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  go []

(* full-string match with Str *)
let matches re s = Str.string_match re s 0 && Str.match_end () = String.length s

let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let url_encode s =
  let buf = Buffer.create (2 * String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' ->
          Buffer.add_char buf c
      | ' ' -> Buffer.add_char buf '+'
      | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Persons and designations                                            *)
(* ------------------------------------------------------------------ *)

type person = { fn : string; occ : int; sn : string }

(* "first name.occ surname", possibly followed by " (i=NNN)" *)
let re_iid = Str.regexp " (i=[0-9]+)$"
let re_person = Str.regexp "\\(.*\\)\\.\\([0-9]+\\) \\(.*\\)"

let strip_iid s =
  try
    let i = Str.search_forward re_iid s 0 in
    String.sub s 0 i
  with Not_found -> s

let parse_person s =
  let s = strip_iid (String.trim s) in
  if matches re_person s then
    let fn = Str.matched_group 1 s in
    let occ = Str.matched_group 2 s in
    let sn = Str.matched_group 3 s in
    Some { fn; occ = int_of_string occ; sn }
  else None

(* fallback when a designation does not parse *)
let person_of s =
  match parse_person s with
  | Some p -> p
  | None -> { fn = String.trim s; occ = 0; sn = "" }

let person_key p =
  String.lowercase_ascii (Printf.sprintf "%s.%d %s" p.fn p.occ p.sn)

let person_designation p =
  if p.sn = "" then p.fn else Printf.sprintf "%s.%d %s" p.fn p.occ p.sn

(* ------------------------------------------------------------------ *)
(* Warnings                                                            *)
(* ------------------------------------------------------------------ *)

type witem =
  | WPerson of person
  | WFam of person * person (* father, mother *)
  | WFamIds of string * string (* possible duplicate families *)

type warning = {
  wtype : string;
  items : witem list; (* first item = main subject *)
  age : int option;
  text : string; (* canonical one-line text, used for dedup + display *)
}

(* --- regexps for every message printed by print_base_warning --------- *)

let re_married = Str.regexp "\\(.*\\) married at age \\([0-9]+\\)"
let re_undef_sex = Str.regexp "Undefined sex for \\(.*\\)"

let re_mother_dead =
  Str.regexp "\\(.*\\) is born after the death of his/her mother \\(.*\\)"

let re_father_dead =
  Str.regexp
    "\\(.*\\) is born more than 2 years after the death of his/her father \
     \\(.*\\)"

let re_parent_after_child =
  Str.regexp "\\(.*\\) born after his/her child \\(.*\\)"

let re_birth_after_death = Str.regexp "\\(.*\\) born after his/her death"
let re_parent_age = Str.regexp "\\(.*\\) was parent at age of \\([0-9]+\\)"

let re_dup_hom =
  Str.regexp
    "possible duplicate families: \\([^ ,]+\\) and \\([^ ,]+\\), \\(.*\\) has \
     unions with several persons named \\(.*\\)"

let re_dup =
  Str.regexp "possible duplicate families: \\([^ ,]+\\) and \\([^ ,]+\\)"

let re_marr_after_death = Str.regexp "\\(.*\\) marriage after his/her death"
let re_marr_before_birth = Str.regexp "\\(.*\\) marriage before his/her birth"

let re_wit_after_death =
  Str.regexp "\\(.*\\) witnessed the \\(.*\\) after his/her death"

let re_wit_before_birth =
  Str.regexp "\\(.*\\) witnessed the \\(.*\\) before his/her birth"

let re_age_diff =
  Str.regexp
    "The difference of age between \\(.*\\) and \\(.*\\) is quite important: \
     \\([0-9]+\\)"

let re_dead_old =
  Str.regexp "\\(.*\\) died at the advanced age of \\([0-9]+\\) years old"

let re_younger_ancestor =
  Str.regexp "\\(.*\\) +has a younger ancestor: \\(.*\\)"

let re_incoherent_sex = Str.regexp "\\(.*\\) sex not coherent with relations.*"

let re_chg_children =
  Str.regexp "Changed order of children of \\(.*\\) and \\(.*\\)"

let re_chg_marriages = Str.regexp "Changed order of marriages of \\(.*\\)"

let re_chg_fam_events =
  Str.regexp "Changed order of family's events for \\(.*\\)"

let re_chg_pers_events =
  Str.regexp "Changed order of person's events for \\(.*\\)"

let re_event_order = Str.regexp "\\(.*\\)'s \\(.*\\) before his/her \\(.*\\)"
let re_title = Str.regexp "\\(.*\\) has incorrect title dates as:"
let mk wtype items age text = Some { wtype; items; age; text }

(* Thresholds to disambiguate messages that are printed identically for
   two different warning constructors. *)
let young_marriage_limit = 50 (* below: YoungForMarriage, else Old *)
let young_parent_limit = 20 (* below: ParentTooYoung, else TooOld  *)

(* Classify a single-line message. IMPORTANT: all Str groups must be
   extracted immediately after a successful match, before calling any
   other function that uses Str (its matching state is global). *)
let classify msg =
  if matches re_undef_sex msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "UndefinedSex" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_married msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    let age = int_of_string g2 in
    let t =
      if age < young_marriage_limit then "YoungForMarriage"
      else "OldForMarriage"
    in
    mk t [ WPerson (person_of g1) ] (Some age) msg
  end
  else if matches re_mother_dead msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "MotherDeadBeforeChildBirth"
      [ WPerson (person_of g1); WPerson (person_of g2) ]
      None msg
  end
  else if matches re_father_dead msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "DeadTooEarlyToBeFather"
      [ WPerson (person_of g1); WPerson (person_of g2) ]
      None msg
  end
  else if matches re_parent_after_child msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "ParentBornAfterChild"
      [ WPerson (person_of g1); WPerson (person_of g2) ]
      None msg
  end
  else if matches re_birth_after_death msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "BirthAfterDeath" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_parent_age msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    let age = int_of_string g2 in
    let t =
      if age < young_parent_limit then "ParentTooYoung" else "ParentTooOld"
    in
    mk t [ WPerson (person_of g1) ] (Some age) msg
  end
  else if matches re_dup_hom msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    let a = min g1 g2 and b = max g1 g2 in
    let text = Printf.sprintf "possible duplicate families: %s and %s" a b in
    mk "PossibleDuplicateFamHomonymous" [ WFamIds (a, b) ] None text
  end
  else if matches re_dup msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    let a = min g1 g2 and b = max g1 g2 in
    let text = Printf.sprintf "possible duplicate families: %s and %s" a b in
    mk "PossibleDuplicateFam" [ WFamIds (a, b) ] None text
  end
  else if matches re_marr_after_death msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "MarriageDateAfterDeath" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_marr_before_birth msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "MarriageDateBeforeBirth" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_wit_after_death msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "PWitnessEventAfterDeath" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_wit_before_birth msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "PWitnessEventBeforeBirth" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_age_diff msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    let g3 = Str.matched_group 3 msg in
    mk "BigAgeBetweenSpouses"
      [ WPerson (person_of g1); WPerson (person_of g2) ]
      (Some (int_of_string g3))
      msg
  end
  else if matches re_dead_old msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "DeadOld" [ WPerson (person_of g1) ] (Some (int_of_string g2)) msg
  end
  else if matches re_younger_ancestor msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "IncoherentAncestorDate"
      [ WPerson (person_of g1); WPerson (person_of g2) ]
      None msg
  end
  else if matches re_chg_children msg then begin
    let g1 = Str.matched_group 1 msg in
    let g2 = Str.matched_group 2 msg in
    mk "ChangedOrderOfChildren" [ WFam (person_of g1, person_of g2) ] None msg
  end
  else if matches re_chg_marriages msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "ChangedOrderOfMarriages" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_chg_fam_events msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "ChangedOrderOfFamilyEvents" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_chg_pers_events msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "ChangedOrderOfPersonEvents" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_incoherent_sex msg then begin
    let g1 = Str.matched_group 1 msg in
    mk "IncoherentSex" [ WPerson (person_of g1) ] None msg
  end
  else if matches re_event_order msg then begin
    (* PEventOrder and FEventOrder print identically; reported as
       PEventOrder (enabled if either is "yes" in the configuration). *)
    let g1 = Str.matched_group 1 msg in
    mk "PEventOrder" [ WPerson (person_of g1) ] None msg
  end
  else mk "Unrecognized" [] None msg

(* --- log parsing (handles multi-line warnings) ----------------------- *)

let parse_log lines =
  let n = Array.length lines in
  let res = ref [] in
  let i = ref 0 in
  while !i < n do
    let line = lines.(!i) in
    if starts_with line "Warning: " then begin
      let msg = String.sub line 9 (String.length line - 9) in
      if msg = "The following children of" && !i + 4 < n then begin
        (* Warning: The following children of
             <father>
           and
             <mother>
           are not in order: | are born very close:
           - child1
           - child2 ... *)
        let father = String.trim lines.(!i + 1) in
        let mother = String.trim lines.(!i + 3) in
        let disc = String.trim lines.(!i + 4) in
        let j = ref (!i + 5) in
        let kids = ref [] in
        while !j < n && starts_with lines.(!j) "- " do
          kids :=
            String.sub lines.(!j) 2 (String.length lines.(!j) - 2) :: !kids;
          incr j
        done;
        let wtype =
          if disc = "are not in order:" then "ChildrenNotInOrder"
          else "CloseChildren"
          (* CloseChildren and DistantChildren print the same text;
             reported as CloseChildren. *)
        in
        let text =
          Printf.sprintf "%s: children of %s and %s: %s"
            (if wtype = "ChildrenNotInOrder" then "children not in order"
             else "children born very close")
            father mother
            (String.concat ", " (List.rev !kids))
        in
        res :=
          {
            wtype;
            items = [ WFam (person_of father, person_of mother) ];
            age = None;
            text;
          }
          :: !res;
        i := !j
      end
      else if matches re_title msg && !i + 1 < n then begin
        let g1 = Str.matched_group 1 msg in
        let title = String.trim lines.(!i + 1) in
        res :=
          {
            wtype = "TitleDatesError";
            items = [ WPerson (person_of g1) ];
            age = None;
            text = msg ^ " " ^ title;
          }
          :: !res;
        i := !i + 2
      end
      else begin
        (match classify msg with Some w -> res := w :: !res | None -> ());
        incr i
      end
    end
    else incr i
  done;
  List.rev !res

(* remove exact duplicates (e.g. duplicate-family warnings are printed
   once per direction and per spouse) *)
let dedup warnings =
  let seen = Hashtbl.create 97 in
  List.filter
    (fun w ->
      let k = w.wtype ^ "|" ^ w.text in
      if Hashtbl.mem seen k then false
      else begin
        Hashtbl.add seen k ();
        true
      end)
    warnings

(* ------------------------------------------------------------------ *)
(* Configuration file: <bases_dir>/<basename>.cfg                      *)
(* ------------------------------------------------------------------ *)

let read_config file =
  let tbl = Hashtbl.create 40 in
  if Sys.file_exists file then
    List.iter
      (fun line ->
        let line = String.trim line in
        if line <> "" && line.[0] <> '#' then
          match String.index_opt line '=' with
          | Some i ->
              let k = String.trim (String.sub line 0 i) in
              let v =
                String.lowercase_ascii
                  (String.trim
                     (String.sub line (i + 1) (String.length line - i - 1)))
              in
              Hashtbl.replace tbl k (v = "yes")
          | None -> ())
      (read_lines file);
  tbl

(* a warning type absent from the configuration defaults to yes *)
let cfg_yes cfg name =
  match Hashtbl.find_opt cfg name with Some b -> b | None -> true

(* some warning types cannot be told apart in the log; they are enabled
   if either configuration entry says yes *)
let enabled cfg wtype =
  match wtype with
  | "CloseChildren" ->
      cfg_yes cfg "CloseChildren" || cfg_yes cfg "DistantChildren"
  | "PEventOrder" -> cfg_yes cfg "PEventOrder" || cfg_yes cfg "FEventOrder"
  | "PWitnessEventAfterDeath" ->
      cfg_yes cfg "PWitnessEventAfterDeath"
      || cfg_yes cfg "FWitnessEventAfterDeath"
  | "PWitnessEventBeforeBirth" ->
      cfg_yes cfg "PWitnessEventBeforeBirth"
      || cfg_yes cfg "FWitnessEventBeforeBirth"
  | "Unrecognized" -> true
  | t -> cfg_yes cfg t

(* ------------------------------------------------------------------ *)
(* Ignored file                                                        *)
(*   person entry:  first_name.occ surname: M, B, D, E                 *)
(*   family entry:  fn.occ sn & fn.occ sn: D, CO, EO                   *)
(*   family entry by ids (duplicates): 4143171 & 4143172: D            *)
(* ------------------------------------------------------------------ *)

type ignored = {
  persons : (string, string list) Hashtbl.t; (* person key -> codes *)
  fams : (string, string list) Hashtbl.t; (* "k1|k2" -> codes *)
  fam_members : (string, string list) Hashtbl.t; (* member key -> codes *)
}

let is_all_digits s =
  s <> ""
  &&
  let ok = ref true in
  String.iter (fun c -> if c < '0' || c > '9' then ok := false) s;
  !ok

let member_key s =
  let s = String.trim s in
  if is_all_digits s then s else person_key (person_of s)

let re_amp = Str.regexp_string " & "

let read_ignored file =
  let ign =
    {
      persons = Hashtbl.create 97;
      fams = Hashtbl.create 97;
      fam_members = Hashtbl.create 97;
    }
  in
  if file <> "" && Sys.file_exists file then
    List.iter
      (fun line ->
        let line = String.trim line in
        if line <> "" && line.[0] <> '#' then
          match String.index_opt line ':' with
          | Some i -> (
              let left = String.trim (String.sub line 0 i) in
              let right =
                String.sub line (i + 1) (String.length line - i - 1)
              in
              let codes =
                List.filter
                  (fun s -> s <> "")
                  (List.map
                     (fun s -> String.uppercase_ascii (String.trim s))
                     (String.split_on_char ',' right))
              in
              match Str.bounded_split re_amp left 2 with
              | [ a; b ] ->
                  let ka = member_key a and kb = member_key b in
                  let key = if ka <= kb then ka ^ "|" ^ kb else kb ^ "|" ^ ka in
                  Hashtbl.replace ign.fams key codes;
                  List.iter
                    (fun k -> Hashtbl.replace ign.fam_members k codes)
                    [ ka; kb ]
              | _ -> Hashtbl.replace ign.persons (member_key left) codes)
          | None -> ())
      (read_lines file);
  ign

(* verification codes: which code covers which warning type *)
let person_code_for = function
  | "YoungForMarriage" | "OldForMarriage" -> Some "M"
  | "ParentTooOld" | "ParentTooYoung" -> Some "B"
  | "DeadOld" -> Some "D"
  | "BigAgeBetweenSpouses" -> Some "E"
  | _ -> None

let fam_code_for = function
  | "PossibleDuplicateFam" | "PossibleDuplicateFamHomonymous" -> Some "D"
  | "ChildrenNotInOrder" | "ChangedOrderOfChildren" | "CloseChildren"
  | "DistantChildren" ->
      Some "CO"
  | "ChangedOrderOfFamilyEvents" | "FEventOrder" | "PEventOrder"
  | "ChangedOrderOfPersonEvents" ->
      Some "EO"
  | _ -> None

let has_code codes c = List.mem c codes

let is_ignored ign w =
  let person_ok =
    match person_code_for w.wtype with
    | None -> false
    | Some code ->
        List.exists
          (function
            | WPerson p -> (
                match Hashtbl.find_opt ign.persons (person_key p) with
                | Some codes -> has_code codes code
                | None -> false)
            | _ -> false)
          w.items
  in
  let fam_ok =
    match fam_code_for w.wtype with
    | None -> false
    | Some code ->
        List.exists
          (function
            | WFamIds (a, b) -> (
                let key = if a <= b then a ^ "|" ^ b else b ^ "|" ^ a in
                match Hashtbl.find_opt ign.fams key with
                | Some codes -> has_code codes code
                | None -> false)
            | WFam (fa, mo) -> (
                let ka = person_key fa and kb = person_key mo in
                let key = if ka <= kb then ka ^ "|" ^ kb else kb ^ "|" ^ ka in
                match Hashtbl.find_opt ign.fams key with
                | Some codes -> has_code codes code
                | None -> false)
            | WPerson p -> (
                (* event-order warnings are per person in the log; accept a
                   family entry naming that person *)
                match Hashtbl.find_opt ign.fam_members (person_key p) with
                | Some codes -> code = "EO" && has_code codes code
                | None -> false))
          w.items
  in
  person_ok || fam_ok

(* ------------------------------------------------------------------ *)
(* Statistics                                                          *)
(* ------------------------------------------------------------------ *)

let all_wtypes =
  [
    "BigAgeBetweenSpouses";
    "BirthAfterDeath";
    "ChangedOrderOfChildren";
    "ChildrenNotInOrder";
    "ChangedOrderOfMarriages";
    "ChangedOrderOfFamilyEvents";
    "ChangedOrderOfPersonEvents";
    "CloseChildren";
    "DeadOld";
    "DeadTooEarlyToBeFather";
    "FEventOrder";
    "FWitnessEventAfterDeath";
    "FWitnessEventBeforeBirth";
    "IncoherentSex";
    "IncoherentAncestorDate";
    "MarriageDateAfterDeath";
    "MarriageDateBeforeBirth";
    "MotherDeadBeforeChildBirth";
    "ParentBornAfterChild";
    "ParentTooOld";
    "ParentTooYoung";
    "PossibleDuplicateFam";
    "PossibleDuplicateFamHomonymous";
    "PEventOrder";
    "PWitnessEventAfterDeath";
    "PWitnessEventBeforeBirth";
    "TitleDatesError";
    "UndefinedSex";
    "YoungForMarriage";
    "OldForMarriage";
    "Unrecognized";
  ]

type stat = { mutable total : int; mutable ignored : int }

let compute_stats warnings ign =
  let tbl = Hashtbl.create 40 in
  let get t =
    match Hashtbl.find_opt tbl t with
    | Some s -> s
    | None ->
        let s = { total = 0; ignored = 0 } in
        Hashtbl.add tbl t s;
        s
  in
  List.iter
    (fun w ->
      let s = get w.wtype in
      s.total <- s.total + 1;
      if is_ignored ign w then s.ignored <- s.ignored + 1)
    warnings;
  tbl

(* "extreme" statistics: each bucket collects its persons *)
type extreme = {
  label : string;
  test : warning -> bool;
  mutable hits : warning list;
}

let make_extremes () =
  let wed_le_11 w =
    w.wtype = "YoungForMarriage"
    && match w.age with Some a -> a <= 11 | None -> false
  in
  let wed_12 w =
    w.wtype = "YoungForMarriage"
    && match w.age with Some a -> a = 12 | None -> false
  in
  let died lo hi w =
    w.wtype = "DeadOld"
    && match w.age with Some a -> a >= lo && a < hi | None -> false
  in
  [
    { label = "Married at age &le; 11"; test = wed_le_11; hits = [] };
    { label = "Married at age 12"; test = wed_12; hits = [] };
    { label = "Died at age 100&ndash;109"; test = died 100 110; hits = [] };
    { label = "Died at age 110&ndash;119"; test = died 110 120; hits = [] };
    { label = "Died at age &ge; 120"; test = died 120 10000; hits = [] };
  ]

(* ------------------------------------------------------------------ *)
(* HTML output                                                         *)
(* ------------------------------------------------------------------ *)

(* Links point back to the base so a name can be corrected in place. With
   wizard access on (-w, the default) the "_w" suffix is added to the base
   name and GeneWeb prompts for the wizard password; with -nw the links are
   plain, suitable for a report that will be shared or served publicly.
   [url] is expected to end with the bare basename. *)
let person_link url p =
  let href =
    Printf.sprintf "%s%s?p=%s&n=%s%s" url
      (if !wizard then "_w" else "")
      (url_encode p.fn) (url_encode p.sn)
      (if p.occ > 0 then Printf.sprintf "&oc=%d" p.occ else "")
  in
  Printf.sprintf "<a href=\"%s\" target=\"_blank\">%s</a>" (html_escape href)
    (html_escape (person_designation p))

let item_html url = function
  | WPerson p -> person_link url p
  | WFam (fa, mo) ->
      Printf.sprintf "%s &amp; %s" (person_link url fa) (person_link url mo)
  | WFamIds (a, b) ->
      Printf.sprintf "families %s &amp; %s" (html_escape a) (html_escape b)

(* sort key: persons by (surname, first name, occ); families by father *)
let item_sort_key = function
  | WPerson p ->
      (String.lowercase_ascii p.sn, String.lowercase_ascii p.fn, p.occ)
  | WFam (fa, _) ->
      (String.lowercase_ascii fa.sn, String.lowercase_ascii fa.fn, fa.occ)
  | WFamIds (a, b) -> (a, b, 0)

let item_uniq_key = function
  | WPerson p -> "p|" ^ person_key p
  | WFam (fa, mo) -> "f|" ^ person_key fa ^ "|" ^ person_key mo
  | WFamIds (a, b) -> "i|" ^ a ^ "|" ^ b

let sort_uniq_items items =
  let tbl = Hashtbl.create 97 in
  let items =
    List.filter
      (fun it ->
        let k = item_uniq_key it in
        if Hashtbl.mem tbl k then false
        else begin
          Hashtbl.add tbl k ();
          true
        end)
      items
  in
  List.sort (fun a b -> compare (item_sort_key a) (item_sort_key b)) items

let generate_html ~basename ~url ~cfg ~stats ~extremes ~kept ~multi oc =
  let pf fmt = Printf.fprintf oc fmt in
  pf "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">\n";
  pf "<title>GeneWeb warnings — %s</title>\n" (html_escape basename);
  pf "<style>\n";
  pf "body{font-family:sans-serif;margin:2em;max-width:70em}\n";
  pf "table{border-collapse:collapse}\n";
  pf "td,th{border:1px solid #999;padding:.25em .6em;text-align:left}\n";
  pf "th{background:#eee}\ntd.num{text-align:right}\n";
  pf "details{margin:.5em 0}\n";
  pf
    "summary{cursor:pointer;display:inline-block;background:#3563a5;color:#fff;padding:.35em \
     .9em;border-radius:.35em;user-select:none}\n";
  pf "summary:hover{background:#274b80}\n";
  pf "ul.plist{columns:3;margin:.5em 0 1em 0}\n";
  pf "ul.plist li{break-inside:avoid}\n";
  pf ".muted{color:#777}\n";
  pf "</style></head><body>\n";
  pf "<h1>GeneWeb warnings — base <i>%s</i></h1>\n" (html_escape basename);

  (* --- statistics ------------------------------------------------- *)
  pf "<h2>Statistics</h2>\n<table>\n";
  pf
    "<tr><th>Warning</th><th>Enabled</th><th>Total</th><th>Ignored \
     (verified)</th><th>Remaining</th></tr>\n";
  List.iter
    (fun t ->
      match Hashtbl.find_opt stats t with
      | None -> ()
      | Some s ->
          pf
            "<tr><td>%s</td><td>%s</td><td class=num>%d</td><td \
             class=num>%d</td><td class=num>%d</td></tr>\n"
            (html_escape t)
            (if enabled cfg t then "yes" else "no")
            s.total s.ignored (s.total - s.ignored))
    all_wtypes;
  pf "</table>\n";

  (* --- extreme statistics ----------------------------------------- *)
  pf "<h2>Extreme statistics</h2>\n";
  List.iter
    (fun e ->
      pf "<details><summary>%s (%d)</summary>\n" e.label (List.length e.hits);
      pf "<ul class=plist>\n";
      let items =
        sort_uniq_items
          (List.concat_map
             (fun w -> match w.items with it :: _ -> [ it ] | [] -> [])
             (List.rev e.hits))
      in
      List.iter (fun it -> pf "<li>%s</li>\n" (item_html url it)) items;
      pf "</ul></details>\n")
    extremes;

  (* --- per-warning-type lists ------------------------------------- *)
  pf "<h2>Persons / families per warning</h2>\n";
  pf
    "<p class=muted>Only warnings enabled (=yes) in %s.cfg and not listed in \
     the ignored file are shown. Lists are unique and sorted; each name opens \
     the person in the base.</p>\n"
    (html_escape basename);
  List.iter
    (fun t ->
      if enabled cfg t then begin
        let ws = List.filter (fun w -> w.wtype = t) kept in
        if ws <> [] then begin
          let items = sort_uniq_items (List.concat_map (fun w -> w.items) ws) in
          pf "<details><summary>%s (%d)</summary>\n" (html_escape t)
            (List.length items);
          pf "<ul class=plist>\n";
          List.iter (fun it -> pf "<li>%s</li>\n" (item_html url it)) items;
          pf "</ul>\n";
          (* full warning texts for context *)
          pf "<details><summary>show full messages (%d)</summary><ul>\n"
            (List.length ws);
          List.iter (fun w -> pf "<li>%s</li>\n" (html_escape w.text)) ws;
          pf "</ul></details>\n";
          pf "</details>\n"
        end
      end)
    all_wtypes;

  (* --- persons with several warnings ------------------------------ *)
  pf "<h2>Persons with several warnings</h2>\n";
  if multi = [] then pf "<p class=muted>none</p>\n"
  else begin
    pf "<table><tr><th>Person</th><th>Warnings</th></tr>\n";
    List.iter
      (fun (p, texts) ->
        pf "<tr><td>%s</td><td>%s</td></tr>\n" (person_link url p)
          (String.concat "<br>" (List.map html_escape texts)))
      multi;
    pf "</table>\n"
  end;
  pf "</body></html>\n"

(* ------------------------------------------------------------------ *)
(* Main                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Arg.parse speclist
    (fun s ->
      if !base_name = "" then base_name := s
      else raise (Arg.Bad ("unexpected argument: " ^ s)))
    usage;
  if !base_name = "" || !log_file = "" then begin
    prerr_endline usage;
    exit 2
  end;
  if !base_url = "" then base_url := "http://localhost:2317/" ^ !base_name;
  if !out_file = "" then out_file := !base_name ^ "_warnings.html";

  let cfg_file = Filename.concat !bases_dir (!base_name ^ ".cfg") in
  let cfg = read_config cfg_file in
  let ign = read_ignored !ignored_file in
  let lines = Array.of_list (read_lines !log_file) in
  let warnings = dedup (parse_log lines) in

  let stats = compute_stats warnings ign in

  (* warnings kept = not ignored *)
  let kept = List.filter (fun w -> not (is_ignored ign w)) warnings in

  (* extremes on kept warnings *)
  let extremes = make_extremes () in
  List.iter
    (fun w ->
      List.iter (fun e -> if e.test w then e.hits <- w :: e.hits) extremes)
    kept;

  (* persons with several (distinct) warnings, among enabled types *)
  let ptbl : (string, person * string list) Hashtbl.t = Hashtbl.create 97 in
  List.iter
    (fun w ->
      if enabled cfg w.wtype then
        List.iter
          (fun it ->
            let ps =
              match it with
              | WPerson p -> [ p ]
              | WFam (fa, mo) -> [ fa; mo ]
              | WFamIds _ -> []
            in
            List.iter
              (fun p ->
                let k = person_key p in
                match Hashtbl.find_opt ptbl k with
                | Some (p0, texts) ->
                    if not (List.mem w.text texts) then
                      Hashtbl.replace ptbl k (p0, texts @ [ w.text ])
                | None -> Hashtbl.add ptbl k (p, [ w.text ]))
              ps)
          w.items)
    kept;
  let multi =
    Hashtbl.fold
      (fun _ (p, texts) acc ->
        if List.length texts >= 2 then (p, texts) :: acc else acc)
      ptbl []
  in
  let multi =
    List.sort
      (fun (a, _) (b, _) ->
        compare
          (String.lowercase_ascii a.sn, String.lowercase_ascii a.fn, a.occ)
          (String.lowercase_ascii b.sn, String.lowercase_ascii b.fn, b.occ))
      multi
  in

  let oc = open_out !out_file in
  generate_html ~basename:!base_name ~url:!base_url ~cfg ~stats ~extremes ~kept
    ~multi oc;
  close_out oc;

  (* console summary *)
  Printf.printf
    "gwwarn: %d log lines, %d warnings after deduplication, %d ignored\n"
    (Array.length lines) (List.length warnings)
    (List.length warnings - List.length kept);
  List.iter
    (fun t ->
      match Hashtbl.find_opt stats t with
      | Some s when s.total > 0 ->
          Printf.printf "  %-32s total %4d  ignored %4d  remaining %4d\n" t
            s.total s.ignored (s.total - s.ignored)
      | _ -> ())
    all_wtypes;
  Printf.printf "report written to %s\n" !out_file

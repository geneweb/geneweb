
type options = {
  base : (string * Gwdb.base) option;
  oc : string * (string -> unit) * (unit -> unit);
}

let speclist (c : options ref) =
  [
    ("-o",
     Arg.String
       (fun s ->
          let oc = open_out s in
          c := { !c with oc = (s, output_string oc, fun () -> close_out oc) }),
     "<GW> output file name (default: stdout)." );
  ]

let default_opts = {
  base = None;
  oc = "", (fun _ -> ()), (fun () -> ());
}

let anonfun c s =
  if !c.base = None then (
    Secure.set_base_dir (Filename.dirname s);
    c := { !c with base = Some (s, Gwdb.open_base s) })
  else raise (Arg.Bad "Cannot treat several databases")

let errmsg = "Usage: " ^ Sys.argv.(0) ^ " <BASE> [OPT]"


type 'a diff = {
  previously : 'a option;
  now : 'a option;
}

type title_diff = {
  t_name : string Def.gen_title_name diff option;
  t_ident : string diff option;
  t_place : string diff option;
  t_date_start : Def.cdate diff option;
  t_date_end : Def.cdate diff option;
  t_nth : int diff option;
}

type person_diff = {
  first_name : Gwdb.istr diff option;
  surname : Gwdb.istr diff option;
  occ : int diff option;
  public_name : Gwdb.istr diff option;
  qualifiers : Gwdb.istr list diff option;
  aliases : Gwdb.istr list diff option;
  first_names_aliases : Gwdb.istr list diff option;
  surnames_aliases : Gwdb.istr list diff option;
  titles : title_diff option;
  (* relations with not native parents *)
  rparents : (Gwdb.iper, Gwdb.istr) Def.gen_relation list diff option;
  (* related persons like (father of witnessed family,
     concerned person of witnessed event, adopted child, etc.) *)
  occupation : Gwdb.istr diff option;
  sex : Def.sex diff option;
  birth : Def.cdate diff option;
  birth_place : Gwdb.istr diff option;
  baptism : Def.cdate diff option;
  baptism_place : Gwdb.istr diff option;
  death : Def.death diff option;
  death_place : Gwdb.istr diff option;
  burial : Def.burial diff option;
  burial_place : Gwdb.istr diff option;
  
}
(*
type family_diff = {
  marriage : cdate;
  marriage_place : 'string;
  marriage_note : 'string;
  marriage_src : 'string;
  witnesses : 'person array;
  relation : relation_kind;
  divorce : divorce;
  fevents : ('person, 'string) gen_fam_event list;
  comment : 'string;
  origin_file : 'string; (* .gw filename where family is defined *)
  fsources : 'string;
  fam_index : 'ifam;
}
*)

module IperSet =
  Set.Make (struct
      type t = Gwdb.iper
      let compare = Gwdb.compare_iper
    end)

module IfamSet =
  Set.Make (struct
      type t = Gwdb.ifam
      let compare = Gwdb.compare_ifam
    end)

let make_diff previously now = {previously; now}

let diff cmp previously now = 
  if cmp previously now <> 0 then
    Some (make_diff (Some previously) (Some now))
  else None

let list_o = function
  | [] -> None
  | l -> Some l

let diff_list cmp previously now =
  let rec aux l1 l2 = match l1, l2 with
    | x :: xs, y :: ys when cmp x y = 0 -> aux xs ys
    | [], [] -> true
    | _ -> false
  in
  if aux previously now then None
  else Some (make_diff (list_o previously) (list_o now))

let diff_string = diff String.compare
let diff_istr = diff Gwdb.compare_istr
let diff_int = diff Int.compare

let diff_to_string to_string diff = match diff with
  | Some diff ->
    let p = Option.map to_string diff.previously in
    let n = Option.map to_string diff.now in
    begin match p, n with
        Some p, Some n -> "P:" ^ p ^ " N:" ^ n
      | Some p, _ -> "P:" ^ p
      | _, Some n -> "N:" ^ n
      | None, None -> "No diff"
    end
  | None -> "No diff"

let date_to_string =
  let starting_char no_num s =
    match s.[0] with
    (*'a'..'z' | 'A'..'Z' | 'à'..'ý' | 'À'..'Ý' *)
    | 'a' .. 'z' | 'A' .. 'Z' | '\xE0' .. '\xFD' | '\xC0' .. '\xDD' -> true
    | '0' .. '9' -> not no_num
    | '?' -> if s = "?" then true else false
    | _ -> false
  in
  let is_printable = function '\000' .. '\031' -> false | _ -> true in
  let gen_correct_string no_num no_colon s =
    let s = String.trim s in
    let rec loop i len =
      if i = String.length s then Buff.get len
      else if len = 0 && not (starting_char no_num s) then
        loop i (Buff.store len '_')
      else
        match s.[i] with
        | ' ' | '\n' | '\t' ->
          if i = String.length s - 1 then Buff.get len
          else loop (i + 1) (Buff.store len '_')
        | '_' | '\\' -> loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
        | ':' when no_colon ->
          let len = Buff.store len '\\' in
          loop (i + 1) (Buff.store (Buff.store len '\\') s.[i])
        | c ->
          let c = if is_printable c then c else '_' in
          loop (i + 1) (Buff.store len c)
    in
    loop 0 0
  in
  let soy y = if y = 0 then "-0" else string_of_int y in
  let print_date_dmy d =
    let add_prec_f = Date.(function
      | About -> fun ds -> "~" ^ ds
      | Maybe -> fun ds -> "?" ^ ds
      | Before -> fun ds -> "<" ^ ds
      | After -> fun ds -> ">" ^ ds
      | Sure -> fun ds -> ds
      | YearInt d2 -> fun ds ->
        ds ^ if d2.month2 = 0 then Printf.sprintf "..%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.sprintf "..%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.sprintf "..%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
      | OrYear d2 -> fun ds ->
        ds ^ if d2.month2 = 0 then Printf.sprintf "|%s" (soy d2.year2)
        else if d2.day2 = 0 then
          Printf.sprintf "|%d/%s" d2.month2 (soy d2.year2)
        else
          Printf.sprintf "|%d/%d/%s" d2.day2 d2.month2 (soy d2.year2)
      )
    in
    let ds =
      let open Date in
      if d.month = 0 then soy d.year
      else if d.day = 0 then Printf.sprintf "%d/%s" d.month (soy d.year)
      else Printf.sprintf "%d/%d/%s" d.day d.month (soy d.year)
    in
    let ds = add_prec_f d.Date.prec ds in
    ds
  in
  let gen_print_date no_colon = function
    | Date.Dgreg (d, Dgregorian) -> print_date_dmy d
    | Dgreg (d, Djulian) ->
      let ds = print_date_dmy (Date.convert ~from:Dgregorian ~to_:Djulian d) in
      Printf.sprintf "%s%s" ds "J"
    | Dgreg (d, Dfrench) ->
      let ds = print_date_dmy (Date.convert ~from:Dgregorian ~to_:Dfrench d) in
      Printf.sprintf "%s%s" ds "F"
    | Dgreg (d, Dhebrew) ->
      let ds = print_date_dmy (Date.convert ~from:Dgregorian ~to_:Dhebrew d) in
      Printf.sprintf "%s%s" ds "H"
    | Dtext t ->
      (* Dans le cas d'une date texte pour un titre, on échappe les ':' *)
      let t = gen_correct_string false no_colon t in
      Printf.sprintf "0(%s)" t
  in
  let print_date = gen_print_date false in
  fun d ->
    match Date.od_of_cdate d with
    | Some d -> print_date d
    | None -> "0"

let diff_person_to_string diff_person =
  let fn_s = diff_to_string Gwdb.string_of_istr diff_person.first_name in
  let sn_s = diff_to_string Gwdb.string_of_istr diff_person.surname in
  let occ_s = diff_to_string string_of_int diff_person.occ in
  let public_s = diff_to_string Gwdb.string_of_istr diff_person.public_name in
  let qualifiers_s = diff_to_string (fun l -> List.map Gwdb.string_of_istr l |> String.concat ";") diff_person.qualifiers in
  let birth_s = diff_to_string date_to_string diff_person.birth in
  "{\nfirst_name: " ^ fn_s ^ "\n" ^
  "surname: " ^ sn_s ^ "\n" ^
  "occ: " ^ occ_s ^ "\n" ^
  "public_name:" ^ public_s ^ "\n" ^
  "qualifiers:" ^ qualifiers_s ^ "\n" ^
  "birth:" ^ birth_s ^ "\n" ^
  "}"

let no_diff_person = {
  first_name = None;
  surname = None;
  occ = None;
  public_name = None;
  qualifiers = None;
  aliases = None;
  first_names_aliases = None;
  surnames_aliases = None;
  titles = None;
  rparents = None;
  occupation = None;
  sex = None;
  birth = None;
  birth_place = None;
  baptism = None;
  baptism_place = None;
  death = None;
  death_place = None;
  burial = None;
  burial_place = None;
}

let diff_istr_list = diff_list Gwdb.compare_istr

let diff_cdate =
  let cmp_cdate d1 d2 = if d1 = d2 then 0 else 1 in
  diff cmp_cdate

let diff_first_name p n =
  diff_istr p.Def.first_name n.Def.first_name
let diff_surname p n =
  diff_istr p.Def.surname n.Def.surname
let diff_occ p n =
  diff_int p.Def.occ n.Def.occ
let diff_public_name p n =
  diff_istr p.Def.public_name n.Def.public_name
let diff_qualifiers p n =
  diff_istr_list p.Def.qualifiers n.Def.qualifiers
let diff_aliases p n =
  diff_istr_list p.Def.aliases n.Def.aliases
let diff_fn_aliases p n =
  diff_istr_list p.Def.first_names_aliases n.Def.first_names_aliases
let diff_sn_aliases p n =
  diff_istr_list p.Def.surnames_aliases n.Def.surnames_aliases
let diff_occupation p n =
  diff_istr p.Def.occupation n.Def.occupation
let diff_sex p n =
  let int_of_sex = function
    | Def.Male -> 0
    | Female -> 1
    | Neuter -> 3
  in
  let cmp_sex s1 s2 = Int.compare (int_of_sex s1) (int_of_sex s2) in
  diff cmp_sex p.Def.sex n.Def.sex
let diff_birth p n =
  diff_cdate p.Def.birth n.Def.birth
let diff_baptism p n =
  diff_cdate p.Def.baptism n.Def.baptism
let diff_death p n =
  let cmp_death d1 d2 = if d1 = d2 then 0 else 1 in
  diff cmp_death p.Def.death n.Def.death
let diff_burial p n =
  let cmp_burial b1 b2 = if b1 = b2 then 0 else 1 in
  diff cmp_burial p.Def.burial n.Def.burial
let diff_birth_place p n =
  diff_istr p.Def.birth_place n.Def.birth_place
let diff_burial_place p n =
  diff_istr p.Def.burial_place n.Def.burial_place
let diff_baptism_place p n =
  diff_istr p.Def.baptism_place n.Def.baptism_place
let diff_death_place p n =
  diff_istr p.Def.death_place n.Def.death_place
let diff_person base p n =
  print_endline @@ "FN: " ^ (Gwdb.string_of_istr p.Def.first_name) ^ " "
                  ^ (Gwdb.string_of_istr n.Def.first_name);
  let diff_fn = diff_istr p.Def.first_name n.Def.first_name in
  let s = diff_to_string (fun istr -> Gwdb.sou base istr) diff_fn in
  print_endline s;
  let diff_sn = diff_istr p.Def.surname n.Def.surname in
  let s = diff_to_string (fun istr -> Gwdb.sou base istr) diff_sn in
  print_endline s;

  let first_name = diff_first_name p n in
  let surname = diff_surname p n in
  let occ = diff_occ p n in
  let public_name = diff_public_name p n in
  let qualifiers = diff_qualifiers p n in
  let aliases = diff_aliases p n in
  let first_names_aliases = diff_fn_aliases p n in
  let surnames_aliases = diff_sn_aliases p n in
  let occupation = diff_occupation p n in
  let sex = diff_sex p n in
  let birth = diff_birth p n in
  let death = diff_death p n in
  let baptism = diff_baptism p n in
  let burial = diff_burial p n in
  let birth_place = diff_birth_place p n in
  let death_place = diff_death_place p n in
  let baptism_place = diff_baptism_place p n in
  let burial_place = diff_burial_place p n in
  let _diffp = no_diff_person in
  let titles = None in
  let rparents = None in
  {
    first_name;
    surname;
    occ;
    public_name;
    qualifiers;
    aliases;
    first_names_aliases;
    surnames_aliases;
    occupation;
    sex;
    birth;
    baptism;
    death;
    burial;
    birth_place;
    baptism_place;
    death_place;
    burial_place;
    titles;
    rparents;
  }
  

module Env : sig
  type t
  val empty : t
  val add_iper : Gwdb.base -> t -> Gwdb.iper -> t
  val add_ifam : Gwdb.base -> t -> Gwdb.ifam -> t
end = struct

  
  type t = {
    persons : IperSet.t;
    families : IfamSet.t;
  }

  let empty = {
    persons = IperSet.empty;
    families = IfamSet.empty;
  }
  

  let debug_add_iper base env iper =
    let p = Gwdb.poi base iper in
    let p' = Gwdb.poi base iper in
    let genp_base = Gwdb.gen_person_of_person_baseonly p in
    let genp_patch = Gwdb.gen_person_of_person p' in
    print_endline (Gwdb.string_of_iper iper);
    let fn, sn = genp_base.first_name, genp_base.surname in
    let fn', sn' = genp_patch.first_name, genp_patch.surname in
    print_endline (Gwdb.sou base fn ^ " " ^ Gwdb.sou base sn);
    print_endline (Gwdb.sou base fn' ^ " " ^ Gwdb.sou base sn');
    
    let dp = diff_person base genp_base genp_patch in
    let dp_s = diff_person_to_string dp in
    print_endline dp_s;
    env
    
  let debug_add_ifam base env ifam =
    let _ = base in
    print_endline (Gwdb.string_of_ifam ifam);
    env

  let add_iper base env iper =
    let _ = base in
    let _ = debug_add_iper base env iper in
    {env with persons = IperSet.add iper env.persons}

  let add_ifam base env ifam =
    let _ = base in
    let _ = debug_add_ifam base env ifam in
    {env with families = IfamSet.add ifam env.families}
end

let updates_from_patch base =
  let _ = base in
  let ipers = Gwdb.ipers_from_patch base in
  let ifams = Gwdb.ifams_from_patch  base in
  let env = Env.empty in
  let env = Gwdb.Collection.fold (Env.add_iper base) env ipers in
  let env = Gwdb.Collection.fold (Env.add_ifam base) env ifams in
  let _ = env in
  ()

let handle_options opts =
  match opts.base with
  | None -> assert false
  | Some (ifile, base) ->
    let _in_dir =
      if Filename.check_suffix ifile ".gwb" then ifile else ifile ^ ".gwb"
    in
    ifile, base

let main () =
  let opts = ref default_opts in
  Arg.parse (speclist opts) (anonfun opts) errmsg;
  let _ifile, base = handle_options !opts in
  Gwdb.set_fpoi_cache base false;
  updates_from_patch base

let _ = Printexc.catch main ()

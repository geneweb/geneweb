
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


let diff_to_string to_string diff =
  match diff with
  | Some diff ->
    let p = Option.map to_string diff.Diff_types.previously in
    let n = Option.map to_string diff.now in
    begin match p, n with
        Some p, Some n -> "P:" ^ p ^ " N:" ^ n
      | Some p, _ -> "P:" ^ p
      | _, Some n -> "N:" ^ n
      | None, None -> "No diff"
    end
  | None -> "No diff"


let diff_person_to_string diff_person =
  let fn_s = diff_to_string Gwdb.string_of_istr diff_person.Diff_types.first_name in
  let sn_s = diff_to_string Gwdb.string_of_istr diff_person.surname in
  let occ_s = diff_to_string string_of_int diff_person.occ in
  let public_s = diff_to_string Gwdb.string_of_istr diff_person.public_name in
  let qualifiers_s = diff_to_string (fun l -> List.map Gwdb.string_of_istr l |> String.concat ";") diff_person.qualifiers in
  let birth_s = diff_to_string Diff_utils.date_to_string diff_person.birth in
  "{\nfirst_name: " ^ fn_s ^ "\n" ^
  "surname: " ^ sn_s ^ "\n" ^
  "occ: " ^ occ_s ^ "\n" ^
  "public_name:" ^ public_s ^ "\n" ^
  "qualifiers:" ^ qualifiers_s ^ "\n" ^
  "birth:" ^ birth_s ^ "\n" ^
  "}"

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

    let gena_base = Gwdb.gen_ascend_of_person_baseonly p in
    let gena_patch = Gwdb.gen_ascend_of_person p' in
    
    print_endline @@ "DBG" ^ (Gwdb.string_of_iper iper);
    let fn, sn = genp_base.first_name, genp_base.surname in
    let fn', sn' = genp_patch.first_name, genp_patch.surname in
    print_endline (Gwdb.sou base fn ^ " " ^ Gwdb.sou base sn);
    print_endline (Gwdb.sou base fn' ^ " " ^ Gwdb.sou base sn');

    begin match gena_base.parents with
    | Some ifam -> print_endline @@ "PARENTS " ^ Gwdb.string_of_ifam ifam
    | None -> print_endline "NO PARENTS"
    end;
    begin match gena_patch.parents with
    | Some ifam -> print_endline @@ "PARENTS " ^ Gwdb.string_of_ifam ifam
    | None -> print_endline "NO PARENTS"
    end;
    
    let dp = Diff_computation.diff_person ~base ~previously:genp_base ~now:genp_patch in
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

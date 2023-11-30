
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


let diff_npoc_to_string diff_npoc = match diff_npoc with
  | Some diff_npoc ->
    let fn_s = diff_to_string Gwdb.string_of_istr diff_npoc.Diff_types.Npoc_diff.first_name in
    let sn_s = diff_to_string Gwdb.string_of_istr diff_npoc.surname in
    let occ_s = diff_to_string string_of_int diff_npoc.occ in
    "{\nfirst_name: " ^ fn_s ^ "\n" ^
    "surname: " ^ sn_s ^ "\n" ^
    "occ: " ^ occ_s ^ "\n}"
  | None -> "nodiff"

let diff_ascends_to_string diff_ascend = match diff_ascend with
  | Some diff_ascend ->
    let fath = diff_npoc_to_string diff_ascend.Diff_types.Ascend_diff.father in
    let moth = diff_npoc_to_string diff_ascend.mother in
    "{\nfather:\n" ^ fath ^ "\n{\nmother:\n" ^ moth
  | None -> "nodiff"

let diff_descend_to_string diff_descend =
  diff_to_string (fun l -> String.concat "," (List.map Gwdb.string_of_iper l)) diff_descend

let diff_person_to_string diff_person =
  let open Diff_types.Person_diff in
  let fn_s = diff_to_string Gwdb.string_of_istr diff_person.first_name in
  let sn_s = diff_to_string Gwdb.string_of_istr diff_person.surname in
  let occ_s = diff_to_string string_of_int diff_person.occ in
  let public_s = diff_to_string Gwdb.string_of_istr diff_person.public_name in
  let qualifiers_s = diff_to_string (fun l -> List.map Gwdb.string_of_istr l |> String.concat ";") diff_person.qualifiers in
  let birth_s = diff_to_string Diff_utils.date_to_string diff_person.birth in
  let ascends_s = diff_ascends_to_string diff_person.ascends in
  let descend_s = diff_descend_to_string diff_person.children in
  "{\nfirst_name: " ^ fn_s ^ "\n" ^
  "surname: " ^ sn_s ^ "\n" ^
  "occ: " ^ occ_s ^ "\n" ^
  "public_name:" ^ public_s ^ "\n" ^
  "qualifiers:" ^ qualifiers_s ^ "\n" ^
  "birth:" ^ birth_s ^ "\n" ^
  "ascends:" ^ ascends_s ^ "\n" ^
  "descend:" ^ descend_s ^ "\n" ^
  "}"

let log = print_endline

module Env : sig
  type t
  val empty : t
  val add_iper : Gwdb.base -> t -> Gwdb.iper -> t
  val add_ifam : Gwdb.base -> t -> Gwdb.ifam -> t
  val fold : ('a -> Gwdb.iper -> 'a) -> 'a -> t -> 'a
end = struct

  
  type t = {
    persons : IperSet.t;
    families : IfamSet.t;
  }

  let empty = {
    persons = IperSet.empty;
    families = IfamSet.empty;
  }
  

  let debug_add_iper _base env iper =

    log @@ (Gwdb.string_of_iper iper);
(*    let p = Gwdb.poi base iper in
    let p' = Gwdb.poi base iper in

    let genp_base = Gwdb.gen_person_of_person_baseonly p in
    let genp_patch = Gwdb.gen_person_of_person p' in

    let gena_base = Gwdb.gen_ascend_of_person_baseonly p in
    let gena_patch = Gwdb.gen_ascend_of_person p' in
    
    log @@ "DBG" ^ (Gwdb.string_of_iper iper);
    let fn, sn = genp_base.first_name, genp_base.surname in
    let fn', sn' = genp_patch.first_name, genp_patch.surname in
    log (Gwdb.sou base fn ^ " " ^ Gwdb.sou base sn);
    log (Gwdb.sou base fn' ^ " " ^ Gwdb.sou base sn');

    begin match gena_base.parents with
    | Some ifam -> log @@ "PARENTS " ^ Gwdb.string_of_ifam ifam
    | None -> log "NO PARENTS"
    end;
    begin match gena_patch.parents with
    | Some ifam -> log @@ "PARENTS " ^ Gwdb.string_of_ifam ifam
    | None -> log "NO PARENTS"
      end;
    
    let dp = Diff_computation.diff_person ~base ~previously:genp_base ~now:genp_patch in
    let dp_s = diff_person_to_string dp in

      log dp_s;*)
    
    env
    
  let debug_add_ifam base env ifam =
    let _ = base in
    log (Gwdb.string_of_ifam ifam);
    env

  let all_ipers_from_ifam base ifam =
    let fam = Gwdb.foi base ifam in
    let fam' = Gwdb.foi base ifam in
    let fath = Gwdb.get_father_baseonly fam in
    let fath' = Gwdb.get_father fam' in
    let moth = Gwdb.get_mother_baseonly fam in
    let moth' = Gwdb.get_mother fam' in
    let children = Gwdb.get_children_baseonly fam in
    let children' = Gwdb.get_children fam' in
    log "FATH";
    log @@ Gwdb.string_of_iper fath;
    log @@ Gwdb.string_of_iper fath';
    log "MOTH";
    log @@ Gwdb.string_of_iper moth;
    log @@ Gwdb.string_of_iper moth';
    log "CHLD";
    Array.iter (fun i -> log @@ Gwdb.string_of_iper i) children;
    log "CHLD'";
    Array.iter (fun i -> log @@ Gwdb.string_of_iper i) children';
    let iset = IperSet.empty in
    let iset = IperSet.add fath iset in
    let iset = IperSet.add moth iset in
    let iset = Array.fold_left (fun iset i -> IperSet.add i iset) iset children in
    let iset = IperSet.add fath' iset in
    let iset = IperSet.add moth' iset in
    let iset = Array.fold_left (fun iset i -> IperSet.add i iset) iset children' in
    iset
  
  let add_all_ipers_from_ifam base env ifam =
    let iperset = all_ipers_from_ifam base ifam in
    {env with persons = IperSet.union env.persons iperset}

  let add_all_related_ipers base env iper =
    let p = Gwdb.poi base iper in
    let p' = Gwdb.poi base iper in
    let fam_p = Gwdb.get_family_baseonly p in
    let fam_n = Gwdb.get_family p' in
    let ifamset = Array.fold_left (fun set ifam -> IfamSet.add ifam set) IfamSet.empty fam_p in
    let ifamset = Array.fold_left (fun set ifam -> IfamSet.add ifam set) ifamset fam_n in
    
    let env = IfamSet.fold (fun ifam env -> add_all_ipers_from_ifam base env ifam) ifamset env in
    env
  
  let add_iper base env iper =
    let _ = debug_add_iper base env iper in
    let env = add_all_related_ipers base env iper in
    {env with persons = IperSet.add iper env.persons}
  
  let add_ifam base env ifam =
    let _ = base in
    let _ = debug_add_ifam base env ifam in
    
    let env = add_all_ipers_from_ifam base env ifam in
    {env with families = IfamSet.add ifam env.families}

  let fold f acc env = IperSet.fold (fun iper acc -> f acc iper) env.persons acc
  
end

let compute_diff base iper =
  let p = Gwdb.poi base iper in
  let p' = Gwdb.poi base iper in

  let genp_base = Gwdb.gen_person_of_person_baseonly p in
  let genp_patch = Gwdb.gen_person_of_person p' in

  log @@ "DBG" ^ (Gwdb.string_of_iper iper);
  let fn, sn = genp_base.first_name, genp_base.surname in
  let fn', sn' = genp_patch.first_name, genp_patch.surname in
  log (Gwdb.sou base fn ^ " " ^ Gwdb.sou base sn);
  log (Gwdb.sou base fn' ^ " " ^ Gwdb.sou base sn');

  let dp = Diff_computation.diff_person ~base ~iper ~previously:genp_base ~now:genp_patch in

  let dp_s = diff_person_to_string dp in

  log dp_s
  

let updates_from_patch base =
  let _ = base in
  let ipers = Gwdb.ipers_from_patch base in
  let ifams = Gwdb.ifams_from_patch  base in
  let env = Env.empty in

  log "=========================IFAMS==================================";
  let env = Gwdb.Collection.fold (Env.add_ifam base) env ifams in

  log "=========================IPERS==================================";
  let env = Gwdb.Collection.fold (Env.add_iper base) env ipers in

  let _ = env in
  
  let _ = Env.fold (fun _acc iper -> compute_diff base iper) () env in
  
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

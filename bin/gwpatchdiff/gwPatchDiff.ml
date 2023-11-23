
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
  first_name : string diff option;
  surname : string diff option;
  occ : int diff option;
  public_name : string diff option;
  qualifiers : string list diff option;
  aliases : string list diff option;
  first_names_aliases : string list diff option;
  surnames_aliases : string list diff option;
  titles : title_diff option;
  (* relations with not native parents *)
  rparents : (Gwdb.iper, string) Def.gen_relation list diff option;
  (* related persons like (father of witnessed family,
     concerned person of witnessed event, adopted child, etc.) *)
  occupation : string diff option;
  sex : Def.sex diff option;
  birth : Def.cdate diff option;
  birth_place : string diff option;
  baptism : Def.cdate diff option;
  baptism_place : string diff option;
  death : Def.death diff option;
  death_place : string diff option;
  burial : Def.burial diff option;
  burial_place : string diff option;
  
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
      let compare i1 i2 = (Obj.magic i1) - (Obj.magic i2)
    end)

module IfamSet =
  Set.Make (struct
      type t = Gwdb.ifam
      let compare i1 i2 = (Obj.magic i1) - (Obj.magic i2)
    end)

module Env : sig
  type t
  val add_iper : Gwdb.iper -> t -> t
  val add_ifam : Gwdb.ifam -> t -> t
end = struct

  
  type t = {
    persons : IperSet.t;
    families : IfamSet.t;
  }
  let add_iper = assert false
  let add_ifam = assert false
end

let main () =
  let opts = ref default_opts in
  Arg.parse (speclist opts) (anonfun opts) errmsg;
  assert false

let _ = Printexc.catch main ()

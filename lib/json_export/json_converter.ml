open Def
module Driver = Geneweb_db.Driver

module type ConverterDriver = sig
  type t

  val str : string -> t
  val int : int -> t
  val obj : (string * t) array -> t
  val null : t
  val array : 't array -> t
  val list : 't list -> t
  val bool : bool -> t
end

(* e.g. js_of_ocaml *)
(* module ConverterDriver = struct
 *   type t = Js.Unsafe.any
 *   let str x = Js.Unsafe.inject @@ Js.string x
 *   let int x = Js.Unsafe.inject x
 *   let obj x = Js.Unsafe.inject @@ Js.Unsafe.obj x
 *   let null = Js.Unsafe.inject Js.null
 *   let array x = Js.Unsafe.inject @@ Js.array x
 *   let list x = array (Array.of_list x)
 *   let bool x = Js.Unsafe.inject @@ if x then Js._true else Js._false
 * end *)

(* e.g. yojson *)
(* module ConverterDriver = struct
 *   type t = Yojson.Basic.t
 *   let str x = `String x
 *   let int x = `Int x
 *   let obj x = `Assoc x
 *   let null = `Null
 *   let array x = `List (Array.to_list x)
 *   let list x = `List x
 *   let bool x = `Bool x
 * end *)

module Make (D : ConverterDriver) = struct
  let str = D.str
  let int = D.int
  let obj = D.obj
  let null = D.null
  let array = D.array
  let list = D.list
  let bool = D.bool
  let opt fn = function Some x -> fn x | None -> null

  (** gwdb to json *)

  let conv_dmy dmy =
    obj
      [|
        ("day", int dmy.day);
        ("delta", if dmy.delta = 0 then null else int dmy.delta);
        ("month", int dmy.month);
        ("year", int dmy.year);
      |]

  let conv_dmy2 dmy =
    obj
      [|
        ("day", int dmy.day2); ("month", int dmy.month2); ("year", int dmy.year2);
      |]

  let conv_date_cal dt cal =
    let prec =
      match dt.prec with
      | Sure -> str "Sure"
      | About -> str "About"
      | Maybe -> str "Maybe"
      | Before -> str "Before"
      | After -> str "After"
      | OrYear _ -> str "OrYear"
      | YearInt _ -> str "YearInt"
    in
    let dmy2 =
      match dt.prec with
      | OrYear dmy2 -> conv_dmy2 dmy2
      | YearInt dmy2 -> conv_dmy2 dmy2
      | _ -> null
    in
    obj
      [|
        ("prec", prec);
        ("dmy1", conv_dmy dt);
        ("dmy2", dmy2);
        ("calendar", str cal);
      |]

  let conv_date oc =
    match oc with
    | Dgreg (d, c) -> conv_date_cal d (Def_show.show_calendar c)
    | Dtext t -> str t

  let conv_cdate cd =
    match Date.od_of_cdate cd with None -> null | Some date -> conv_date date

  let conv_pevent_name x =
    str
    @@ Def_show.show_gen_pers_event_name
         (fun fmt -> Format.fprintf fmt "Epers_Name %s")
         x

  let conv_event_witness_kind x = str @@ Def_show.show_witness_kind x
  let handler_of_iper i = str @@ Driver.Iper.to_string i
  let handler_of_ifam i = str @@ Driver.Ifam.to_string i

  let conv_event_witness (i, kind) =
    obj
      [|
        ("person", handler_of_iper i); ("kind", conv_event_witness_kind kind);
      |]

  let conv_pevent pevent =
    obj
      [|
        ("place", str pevent.epers_place);
        ("reason", str pevent.epers_reason);
        ("note", str pevent.epers_note);
        ("src", str pevent.epers_src);
        ("name", conv_pevent_name pevent.epers_name);
        ("date", conv_cdate pevent.epers_date);
        ( "witnesses",
          array @@ Array.map conv_event_witness pevent.epers_witnesses );
      |]

  let conv_title_name = function
    | Tmain -> str ""
    | Tname s -> str s
    | Tnone -> null

  let conv_title gen_title =
    obj
      [|
        ("name", conv_title_name gen_title.t_name);
        ("date_start", conv_cdate gen_title.t_date_start);
        ("date_end", conv_cdate gen_title.t_date_end);
        ("nth", int gen_title.t_nth);
        ("ident", str gen_title.t_ident);
        ("place", str gen_title.t_place);
      |]

  let conv_relation_kind x = str @@ Def_show.show_relation_kind x

  let conv_fevent_name x =
    str
    @@ Def_show.show_gen_fam_event_name
         (fun fmt -> Format.fprintf fmt "Efam_Name %s")
         x

  let conv_fevent fevent =
    obj
      [|
        ("date", conv_cdate fevent.efam_date);
        ("name", conv_fevent_name fevent.efam_name);
        ("note", str fevent.efam_note);
        ("place", str fevent.efam_place);
        ("reason", str fevent.efam_reason);
        ("src", str fevent.efam_src);
        ( "witnesses",
          array @@ Array.map conv_event_witness fevent.efam_witnesses );
      |]

  let conv_divorce = function
    | NotDivorced -> bool false
    | Divorced date -> conv_cdate date
    | NotSeparated -> bool false
    | Separated_old -> bool true
    | Separated date -> conv_cdate date

  let conv_relation_type x = str @@ Def_show.show_relation_type x

  let conv_rparent gen_relation =
    obj
      [|
        ("father", opt handler_of_iper gen_relation.r_fath);
        ("mother", opt handler_of_iper gen_relation.r_moth);
        ("source", str gen_relation.r_sources);
        ("type", conv_relation_type gen_relation.r_type);
      |]

  let conv_death = function
    | Def.NotDead -> str "NotDead"
    | Death (Killed, _) -> str "Killed"
    | Death (Murdered, _) -> str "Murdered"
    | Death (Executed, _) -> str "Executed"
    | Death (Disappeared, _) -> str "Disappeared"
    | Death (Unspecified, _) -> str "Unspecified"
    | DeadYoung -> str "DeadYoung"
    | DeadDontKnowWhen -> str "DeadDontKnowWhen"
    | DontKnowIfDead -> str "DontKnowIfDead"
    | OfCourseDead -> str "OfCourseDead"

  let conv_person base p =
    let pp = Driver.gen_person_of_person p in
    let pp = Futil.map_person_ps (fun i -> i) (Driver.sou base) pp in
    let pa = Driver.gen_ascend_of_person p in
    let pu = Driver.gen_union_of_person p in
    obj
      [|
        ( "access",
          int (match pp.access with Private -> 2 | Public -> 1 | _ -> 0) );
        ("aliases", list (List.map str pp.aliases));
        ("first_names_aliases", list (List.map str pp.first_names_aliases));
        ("firstname", str pp.first_name);
        ("image", str pp.image);
        ("iper", handler_of_iper pp.key_index);
        ("lastname", str pp.surname);
        ("note", str pp.notes);
        ("occ", int pp.occ);
        ("occupation", str pp.occupation);
        ("parents", opt handler_of_ifam pa.parents);
        ( "consang",
          if pa.consang = Adef.no_consang then null
          else int (Adef.fix_repr pa.consang) );
        ("pevents", list (List.map conv_pevent pp.pevents));
        ("psources", str pp.psources);
        ("public_name", str pp.public_name);
        ("qualifiers", list (List.map str pp.qualifiers));
        ("related", list (List.map handler_of_iper pp.related));
        ("rparents", list (List.map conv_rparent pp.rparents));
        ("sex", int (match pp.sex with Male -> 1 | Female -> 2 | _ -> 0));
        ("surnames_aliases", list (List.map str pp.surnames_aliases));
        ("titles", list (List.map conv_title pp.titles));
        ("unions", list (List.map handler_of_ifam (Array.to_list pu.family)));
      |]

  let conv_family base f =
    let ff = Driver.gen_family_of_family f in
    let ff =
      Futil.map_family_ps (fun i -> i) (fun i -> i) (Driver.sou base) ff
    in
    let fc = Driver.gen_couple_of_family f in
    let fd = Driver.gen_descend_of_family f in
    obj
      [|
        ("fevents", list (List.map conv_fevent ff.fevents));
        ("comment", str ff.comment);
        ("origin_file", str ff.origin_file);
        ("fsources", str ff.fsources);
        ("witnesses", array (Array.map handler_of_iper ff.witnesses));
        ("children", array (Array.map handler_of_iper fd.children));
        ("parents", array (Array.map handler_of_iper @@ Adef.parent_array fc));
      |]
end

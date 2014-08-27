(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)
(*
   Copyright 2009, 2010, 2011 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


module C = Piqi_common
open C


module Idtable = Piqi_db.Idtable
type idtable = T.piqdef Idtable.t


(* start in boot_mode by default, it will be switched off later (see below) *)
let boot_mode = ref true

(* resolved type definition for the Piqi language;
 * it will be appropriately initialized during boot stage (see below) *)
let piqi_lang_def :T.piqtype ref = ref `bool
(* resolved type definition for the Piqi specification *)
let piqi_spec_def :T.piqtype ref = ref `bool

(* resolved "piqdef" type definition
 * Will be appropriately initialized during boot stage (see below) *)
let piqdef_def :T.piqtype ref = ref `bool


(* processing hooks to be run at the end of Piqi module load & processing *)
let processing_hooks = ref []

let register_processing_hook (f :idtable -> T.piqi -> unit) =
  debug "register_processing_hook(0)\n";
  (* NOTE: create an empty idtable just to make invocations below work; none of
   * the plugins actually require a valid idtable to exist at this point, so we
   * don't care *)
  let idtable = Idtable.empty in
  (* run the hook on the embedded Piqi self-specification *)
  f idtable T.boot_piqi; (* XXX: some_of !boot_piqi *)
  debug "register_processing_hook(1.5)\n";
  f idtable T.piqi_lang;
  debug "register_processing_hook(1.6)\n";
  f idtable T.piqi_spec;
  debug "register_processing_hook(1)\n";
  (* add the hook to the list of registered hooks *)
  processing_hooks := !processing_hooks @ [f]


let add_piqdef idtable (piqdef:T.piqdef) = 
  let name = piqdef_name piqdef in
  debug "add_piqdef: %s\n" name;
  if Idtable.mem idtable name
  then
    let prev_def = Idtable.find idtable name in
    error piqdef
      ("duplicate type definition " ^ quote name ^ "\n" ^
       error_string prev_def "first defined here")
  else
    Idtable.add idtable name piqdef


let add_piqdefs idtable defs =
  List.fold_left add_piqdef idtable defs


let add_imported_piqdef idtable (piqdef:T.piqdef) =
  let open Import in
  let import = 
    match get_parent piqdef with
      | `import x -> x
      | _ -> assert false
  in
  (* while adding imported defs to the idtable, transform definition's names to
   * contain module's namespace *)
  let name = some_of import.name ^ "/" ^ piqdef_name piqdef in
  debug "add_imported_piqdef: %s\n" name;
  Idtable.add idtable name piqdef


let add_imported_piqdefs idtable defs =
  List.fold_left add_imported_piqdef idtable defs


let find_def idtable name =
  try Idtable.find idtable name
  with Not_found ->
    error name ("unknown type " ^ quote name)


let resolve_typeref map t =
  match t with
    | `name name ->
        let def = find_def map name in
        (def: T.piqdef :> T.typeref)
    | _ -> t (* already resolved *)


(* XXX: is there a way to avoid code duplicaton here? *)
let resolve_field_typeref map f =
  let open F in
  match f.typeref with
    | None -> () (* flag *)
    | Some t ->
        f.typeref <- Some (resolve_typeref map t)


let resolve_option_typeref map o =
  let open O in
  match o.typeref with
    | None -> ()
    | Some t ->
        o.typeref <- Some (resolve_typeref map t)


let resolve_typerefs map = function
  | `record r ->
      List.iter (resolve_field_typeref map) r.R#field
  | `variant v ->
      List.iter (resolve_option_typeref map) v.V#option
  | `alias a ->
      a.A#typeref <- resolve_typeref map a.A#typeref
  | `list l ->
      l.L#typeref <- resolve_typeref map l.L#typeref
  | _ -> ()


let check_name x =
  if not (Piqi_name.is_valid_name x)
  then error x ("invalid name: " ^ quote x)
  else ()


let check_scoped_name x =
  if not (Piqi_name.is_valid_scoped_name x)
  then error x ("invalid scoped name: " ^ quote x)
  else ()


let check_opt_name = function
  | None -> ()
  | Some x -> check_name x


let check_dup_names what names =
  match find_dups names with
    | None -> ()
    | Some (name, prev) ->
        error name
          ("duplicate " ^ what ^ " name " ^ quote name ^ "\n" ^
            error_string prev "first defined here")


let check_typeref obj (t:T.typeref) =
  match t with 
    | `name x -> check_scoped_name x
    | #T.piqdef ->
        error obj "use of type definition as type is prohibited"
    | _ -> ()


let error_noboot obj s =
  if !boot_mode || !Config.noboot
  then ()
  else error obj s


let check_no_builtin_type obj t =
  match t with 
    | #T.piqdef | `name _ -> check_typeref obj t
    | _ ->
        error_noboot obj "use of built-in types is allowed only from boot files or when running in \"noboot\" mode"


let check_field f =
  let open Field in
  begin
    begin
    check_opt_name f.name;
    match f.name, f.typeref with
      | None, None ->
          error f "name or type must be specified for a field"
      | _, Some t ->
          check_no_builtin_type f t
      | Some _, None -> (* flag *)
          begin
            (if f.mode <> `optional
            then error f "flags must be optional");

            (if f.default <> None
            then error f "flags may not specify default")
          end
    end;

    if f.default <> None && f.mode <> `optional
    then
      error f.default "default values may only be specified for optional fields"
  end


let check_record r =
  let fields = r.R#field in
  (* XXX: Protobuf doesn't print any warnings on records with no fields *)
  (*
  if fields = []
  then warning r ("record " ^ quote r.R#name ^ " doesn't specify any fields");
  *)
  List.iter check_field fields


let check_option o =
  let open Option in
  begin
    check_opt_name o.name;
    match o.name, o.typeref with
      | _, Some t ->
          check_no_builtin_type o t
      | None, None ->
          error o "name or type must be specified for an option"
      | _ -> ()
  end


let check_variant v =
  (* TODO: check for co-variant loops *)
  let name = v.V#name in
  let options = v.V#option in
  if options = []
  then error v ("variant " ^ quote name ^ " doesn't specify any options");
  List.iter check_option options


let check_enum_option x =
  let open O in
  begin
    (* consts name must be defined and types must not *)
    if x.name = None
    then error x ("enum options must specify name");

    if x.typeref <> None
    then error x ("enum options must not specify type");

    check_opt_name x.name;
  end


let check_enum e =
  let name = e.E#name in
  let options = e.E#option in
  if options = []
  then error e ("enum " ^ quote name ^ " doesn't specify any options");
  List.iter check_enum_option options


let check_wire_type a wt =
  let t = unalias (`alias a) in
  match wt with
    | `varint | `zigzag_varint | `fixed32 | `fixed64 
    | `signed_varint | `signed_fixed32 | `signed_fixed64 when t = `int -> ()
    | `fixed32 | `fixed64 when t = `float -> ()
    | _ ->
        error a ("wire type " ^ quote (Piqi_wire.wire_type_name wt) ^
                 " is incompatible with piq type " ^ quote (piqi_typename t))


let check_alias a =
  let open A in
  begin
    check_typeref a a.typeref;
    (*
    check_no_builtin_type a a.typeref;
    *)
  end


let check_list l =
  let open L in
  begin
    check_no_builtin_type l l.typeref
  end


let check_def def =
  check_name (piqdef_name def);
  match def with
    | `record x -> check_record x
    | `variant x -> check_variant x
    | `enum x -> check_enum x
    | `alias x -> check_alias x
    | `list x -> check_list x


let check_resolved_alias a = 
  (* TODO: check for alias loops *)
  let open A in
  begin
    (* check for wire-types compatibility with piq types *)
    match a.wire_type with
      | None -> ()
      | Some x ->
          check_wire_type a x
    (* XXX: prohibit wire type overrides in upper-level aliases? (currently they
     * are silently ignored *)
  end


let check_resolved_def def =
  match def with
    | `record x ->
        let names = List.map (fun x -> name_of_field x) x.R#field in
        check_dup_names "field" names
    | `variant x | `enum x ->
        let names = List.map (fun x -> name_of_option x) x.V#option in
        check_dup_names "option" names
    | `alias x ->
        check_resolved_alias x
    | _ -> ()


let check_extension x =
  let open Extend in
  begin
    if x.name = []
    then error x ("extension doesn't specify any names");

    if x.quote = []
    then error x ("extension doesn't specify any extensions");

    List.iter
      (fun x ->
        if Piqi_name.has_parent x
        then
          error x "extensions of imported defintions are not supported yet")
      x.name;

    List.iter check_scoped_name x.name;
  end


let debug_loc prefix =
  debug "%s out count = %d, in count = %d\n" prefix !Piqloc.ocount !Piqloc.icount


let assert_loc () =
  if (!Piqloc.ocount <> !Piqloc.icount)
  then
    failwith
     (Printf.sprintf "internal_error: out count = %d, in count = %d\n" !Piqloc.ocount !Piqloc.icount)


(* convert textobj, i.e. JSON or XML to piqobj -- this function will be set by
 * either Piqi_json or Piqi_xml, depending on which format we are dealing with
 * at the moment *)
let piqobj_of_ref_init (piqtype: T.piqtype) (text: int) :Piqobj.obj =
  assert false

let piqobj_of_ref = ref piqobj_of_ref_init


let resolve_default_value default piqtype piqobj =
    assert_loc ();
    debug_loc "resolve_default_value(0)";
    let binobj = Piqobj_to_wire.gen_binobj piqobj in

    (* NOTE: fixing (preserving) location counters which get skewed during
     * parsing defaults *)
    Piqloc.icount := !Piqloc.ocount;
    debug_loc "resolve_default_value(1)";

    default.T.Any.binobj <- Some binobj;
    default.T.Any.typename <- Some (C.full_piqi_typename piqtype);
    ()


let resolve_field_default x =
  (* debug "resolve_field_default: %s\n" (C.name_of_field x); *)
  let open F in
  match x.default, x.typeref with
    | None, _ -> () (* no default *)
    | Some {T.Any.binobj = Some _}, _ ->
        (* nothing to do -- object is already resolved *)
        ()
    | Some ({T.Any.ast = Some ast} as default), Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = Piqobj_of_piq.parse_obj piqtype ast in
        resolve_default_value default piqtype piqobj

    | Some ({T.Any.ref = Some ref} as default), Some typeref ->
        let piqtype = C.piqtype typeref in
        let piqobj = !piqobj_of_ref piqtype ref in
        resolve_default_value default piqtype piqobj

    | _, None -> () (* there is no default for a flag *)
    | _ ->
        assert false (* either binobj or ast or textobj must be defined *)


let resolve_defaults = function
  | `record x ->
      List.iter resolve_field_default x.R#field
  | _ -> ()


let copy_obj (x:'a) :'a =
  Obj.obj (Obj.dup (Obj.repr x))
let copy_obj x = reference copy_obj x


let copy_obj_list l = List.map copy_obj l
let copy_obj_list l = reference copy_obj_list l


let copy_variant ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x V#{ x with option = copy_obj_list x.option }
  else copy_obj x


let copy_record ?(copy_parts=true) x =
  if copy_parts
  then Piqloc.addrefret x R#{ x with field = copy_obj_list x.field }
  else copy_obj x


let copy_def ~copy_parts (x:T.piqdef) =
  let res =
    match x with
      | `record x -> `record (copy_record ~copy_parts x)
      | `variant x -> `variant (copy_variant ~copy_parts x)
      | `enum x -> `enum (copy_variant ~copy_parts x)
      | `alias x -> `alias (copy_obj x)
      | `list x -> `list (copy_obj x)
  in
  (* preserve location information *)
  Piqloc.addrefret x res


let copy_defs ?(copy_parts=true) defs = List.map (copy_def ~copy_parts) defs


let copy_imports l = List.map copy_obj l


let resolve_defs ?piqi idtable (defs:T.piqdef list) =
  (*
  (* a fresh copy of defs is needed, since we can't alter the original ones:
   * we need to resolve types & assign codes in order to resolve_defaults *)
  *)

  (* check definitions validity *)
  List.iter check_def defs;

  (* add definitions to the map: def name -> def *)
  let idtable = add_piqdefs idtable defs in

  (* resolve type references using the map *)
  List.iter (resolve_typerefs idtable) defs;

  (* check records, variants, enums for duplicate field/option names; check wire
   * types in aliases *)
  List.iter check_resolved_def defs;

  (* assign wire codes, if they are unassigned; check otherwise; check
   * correctness of .wire-packed usage *)
  Piqi_wire.process_defs defs;

  (* set up parent namespace to local piqi defs *)
  (match piqi with
    | Some piqi ->
        List.iter (fun def -> set_parent def (`piqi piqi)) defs;
    | None -> ()
  );

  (* return updated idtable *)
  idtable


let check_defs idtable defs =
  ignore (resolve_defs idtable (copy_defs defs))


let read_piqi_common fname piq_parser :T.ast =
  (* don't expand abbreviations until we construct the containing object *)
  let res = Piq_parser.read_all piq_parser ~expand_abbr:false in

  if res = []
  then piqi_warning ("piqi file is empty: " ^ fname);

  (* wrapping items in list to make them contents of "piqi" record *)
  let res = `list res in
  let startloc = (fname, 1, 1) in (* start location *)
  let ast = Piqloc.addlocret startloc res in
  (* now expand abbreviations *)
  Piq_parser.expand ast


let read_piqi_channel fname ch :T.ast =
  (* XXX: handle read errors *)
  let piq_parser = Piq_parser.init_from_channel fname ch in
  read_piqi_common fname piq_parser


let read_piqi_string fname content :T.ast =
  let piq_parser = Piq_parser.init_from_string fname content in
  read_piqi_common fname piq_parser


let open_piqi fname =
  try Pervasives.open_in_bin fname
  with Sys_error s ->
    piqi_error ("error opening piqi file: " ^ s)


let read_piqi_file fname :T.ast =
  let ch = open_piqi fname in
  let res =
    try read_piqi_channel fname ch
    with x -> (* try ... after *)
      Pervasives.close_in ch;
      raise x
  in
  Pervasives.close_in ch;
  res


let check_modname x =
  if Piqi_name.is_valid_modname x
  then ()
  else error x ("invalid piqi module name: " ^ x)


let check_assign_module_name ?modname fname (piqi:T.piqi) =
  let open P in
  match piqi.modname, modname with
    | Some x, Some x' ->
        check_modname x;
        (* check that the requested module name corresponds to the module name
         * defined in the file *)
        if x <> x'
        then
          error piqi
            ("module loaded as " ^ quote x' ^ 
             " has different name " ^ quote x)
        else ()
    | Some x, None -> (* name is already defined for the module *)
        check_modname x
    | None, Some x -> 
        piqi.modname <- modname
    | None, None ->
        (* basename + chop all extensions + underscores to dashes *)
        let basename = Piqi_file.basename fname in
        let name = Piqi_name.make_local_name basename in
        if Piqi_name.is_valid_name name
        then piqi.modname <- Some name
        else error piqi "piqi module name can not be derived from the file name"


(* XXX: demand explicit import name to avoid potential problems after renaming
 * imported module name? *)
let assign_import_name x =
  let open Import in
  match x.name with
    | Some x -> (* import name is already defined *)
        check_name x
    | None ->
        (* derive import name from the original module's name *)
        let name = Piqi_name.get_local_name x.modname in
        x.name <- Some name


let mlobj_to_piqobj piqtype wire_generator mlobj =
  debug_loc "mlobj_to_piqobj(0)";
  assert_loc ();
  let binobj = Piqirun.gen_binobj wire_generator mlobj in
  debug_loc "mlobj_to_piqobj(1.5)";

  (* dont' resolve defaults when reading wire *)
  let piqobj =
    C.with_resolve_defaults false (Piqobj_of_wire.parse_binobj piqtype) binobj
  in
  debug_loc "mlobj_to_piqobj(1)";
  assert_loc ();

  piqobj


let mlobj_to_ast piqtype wire_generator mlobj =
  debug_loc "mlobj_to_ast(0)";
  let piqobj = mlobj_to_piqobj piqtype wire_generator mlobj in
  debug_loc "mlobj_to_ast(1.5)";
  let ast = Piqobj_to_piq.gen_obj piqobj in
  debug_loc "mlobj_to_ast(1)";
  assert_loc ();
  ast


let mlobj_of_piqobj wire_parser piqobj =
  let binobj = Piqobj_to_wire.gen_binobj piqobj in
  let mlobj = Piqirun.parse_binobj wire_parser binobj in
  mlobj


let mlobj_of_ast piqtype wire_parser ast =
  debug_loc "mlobj_of_ast(0)";

  (*
  (* initialize starting location code *)
  let max_count = max !T.icount !T.ocount in
  T.icount := max_count;
  T.ocount := max_count;
  *)

  (* XXX: find a better way to set this option *)
  Piqobj_of_piq.delay_unknown_warnings := true;

  (* We have to resolve defaults while reading piqi in order to provide correct
   * location bindings. It is not possible to "fix" skewed location bindings
   * in piqtype.ml after default values get parsed. We rather decided to fix
   * location bindings here -- see resolve_defaults function for details *)
  let piqobj =
    C.with_resolve_defaults true (Piqobj_of_piq.parse_obj piqtype) ast
  in
  debug_loc "mlobj_of_ast(1.5)";

  Piqobj_of_piq.delay_unknown_warnings := false;

  let mlobj = mlobj_of_piqobj wire_parser piqobj in
  debug_loc "mlobj_of_ast(1)";
  assert_loc ();

  mlobj


let parse_piqi ast =
  (* XXX: handle errors *)
  debug "parse_piqi(0)\n";
  (* use prepared static "piqi" definition to parse the ast *)
  let res = mlobj_of_ast !piqi_lang_def T.parse_piqi ast in
  debug "parse_piqi(1)\n";
  res


(* get the list of unique piqi includes by traversing recursively through
 * include tree; the input piqi module will be put as the last element of the
 * list *)
let get_includes piqi =
  (* get the list of all included modules *)
  let l = flatmap (fun x ->
    let res = x.P#included_piqi in
    if res = [] (* the module is loaded, but hasn't been processed yet *)
    then error x "included piqi modules form a loop"
    else res) piqi.P#included_piqi in

  (* remove duplicates -- one module may be included from many modules *)
  let l = uniqq l in

  (* simple check for includes loop; provides very limited diagnostic *)
  if List.memq piqi l
  then error piqi "included piqi modules form a loop";

  (* put the original (input) piqi module at the last position of the list *)
  l @ [piqi]


let get_piqdefs modules =
  flatmap (fun x -> x.P#piqdef) modules


let get_extensions modules =
  flatmap (fun x -> x.P#extend) modules


let get_imports modules =
  flatmap (fun x -> x.P#import) modules


let get_resolved_imports modules =
  flatmap (fun x -> x.P#resolved_import) modules


let get_custom_fields modules =
  let l = flatmap (fun x -> x.P#custom_field) modules in
  uniq l


let is_unknown_field custom_fields x =
  match x with
    | `named {T.Named.name = name} | `name name ->
        if List.mem name custom_fields
        then false (* field is a custom field, i.e. "known" *)
        else true
    | _ -> true


let check_unknown_fields ?prepend unknown_fields custom_fields =
  let unknown_fields =
    List.filter (is_unknown_field custom_fields) unknown_fields
  in

  let warn x =
    (* call the function for printing prepending warning message *)
    (match prepend with
      | Some f -> f ()
      | None -> ());
    Piqobj_of_piq.warn_unknown_field x
  in

  (* print warnings *)
  List.iter warn unknown_fields


let check_imports piqi =
  let rec check_dups = function
    | [] -> ()
    | h::t ->
        begin
          if List.exists (fun x -> h.Import#name = x.Import#name) t
          then error h ("duplicate import name " ^ quote (some_of h.Import#name));

          if List.exists (fun x -> h.Import#piqi == x.Import#piqi) t
          then warning h ("duplicate import module " ^ quote h.Import#modname);

          check_dups t
        end
  in
  check_dups piqi.P#resolved_import;

  (* get the list of all imported modules *)
  let l = List.map (fun x -> some_of x.Import#piqi) piqi.P#resolved_import in
  (* simple check for import loops; provides very limited diagnostic *)
  (* NOTE: import loops disallowed in Protobuf as well *)
  if List.memq piqi l
  then error piqi "imported piqi modules form a loop"


(* resolve extension names to correspondent piqdefs *)
let process_extension idtable x =
  let open Extend in
  let names = x.name in
  List.map (fun name -> (find_def idtable name, x)) names


(* From (key, value) list extract values for the specified key, and return the
 * list of extracted values and the remaining tuples; the order of both values
 * and remaining items is preserved *)
(* NOTE: not tail recursive *)
(* NOTE: using reference-based eq function (==) *)
let takeout key l =
  let rec aux values rem = function
    | [] -> List.rev values, List.rev rem
    | (key', value)::t when key' == key ->
        aux (value::values) rem t
    | h::t ->
        aux values (h::rem) t
  in aux [] [] l
    

(* group unsorted (key, value) pairs by their first element (key) and return
 * the list of groups containing (key, list of values) for each group *)
(* NOTE: using reference-based eq function (==) *)
let group_pairs l =
  let rec aux groups = function
    | [] -> List.rev groups
    | (key, value)::t ->
        let values, rem = takeout key t in
        aux ((key, value::values)::groups) rem
  in aux [] l


let apply_extensions piqdef extensions custom_fields =
  let trace' = !Piqloc.trace in
  (* Piqloc.trace := false; *)
  debug "apply_extensions(0)\n";
  let piqdef_ast = mlobj_to_ast !piqdef_def T.gen__piqdef piqdef in
  let extension_entries =
    List.concat (List.map (fun x -> x.Extend#quote) extensions)
  in
  let extension_asts = List.map (fun x -> some_of x.Any#ast) extension_entries in
  let extended_piqdef_ast =
    match piqdef_ast with
     | `named ({T.Named.value = ((`list l) as _ref)} as x) ->
         let v = `list (l @ extension_asts) in

         let v = Piq_parser.piq_addrefret _ref v in

         let res = `named {x with T.Named.value = v} in

         ignore (Piq_parser.piq_addrefret x res);

         res

     | _ ->
         (* extensions can only be applied to named containers and all of
          * piqdefs are named containers *)
         assert false
  in
  let context_str = "while applying extensions to this definition ..." in
  debug "apply_extensions(1)\n";
  let extended_piqdef =
    try
      mlobj_of_ast !piqdef_def T.parse_piqdef extended_piqdef_ast
    with (C.Error _) as e ->
      (* TODO, XXX: one error line is printed now, another (original) error
       * later -- it is inconsistent *)
      (
        prerr_endline (C.error_string piqdef context_str);
        (* re-raise the original exception after printing some context info *)
        raise e
      )
  in
  debug "apply_extensions(2)\n";
  Piqloc.trace := trace';

  (* get unparsed extension fields fields *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in
  check_unknown_fields unknown_fields custom_fields
    ~prepend:(fun () -> C.warning piqdef context_str);

  extended_piqdef


(* expand extensions, i.e. extend exising definitions with extensions *)
let expand_extensions defs extensions custom_fields =
  let idtable = Idtable.empty in
  let idtable = add_piqdefs idtable defs in
  (* get a list of (piqdef, extensions) pairs from all extensiosn by resolving
   * extension names to piqdefs *)
  let l = flatmap (process_extension idtable) extensions in
  (* group the list of extensions by piqdef obtaining the list of
   * (piqdef, [extension]) pairs *)
  let groups = group_pairs l in
  let extended_defs =
    List.map (fun (piqdef, extensions) ->
      apply_extensions piqdef extensions custom_fields) groups
  in
  let involved_defs = List.map (fun (def, _) -> def) groups in
  let untouched_defs =
    List.filter (fun x -> not (List.memq x involved_defs)) defs
  in
  untouched_defs @ extended_defs


let get_imported_defs imports =
  let aux x = 
    let piqi = some_of x.Import#piqi in
    (* in order to avoid conflict between local defs and also defs imported
     * several times, creating a shallow copy of imported defs just to be able
     * to safely mutate the "parent" field *)
    let imported_defs = copy_defs piqi.P#resolved_piqdef ~copy_parts:false in
    (* set parent namespace for imported definitions *)
    List.iter (fun def -> set_parent def (`import x)) imported_defs;
    imported_defs
  in
  flatmap aux imports


let get_function_defs_init piqi = []

let get_function_defs = ref get_function_defs_init


(* do include & extension expansion for the loaded piqi using extensions from
 * all included piqi modules *)
let rec process_piqi ?modname ?(cache=true) ?(fname="") (piqi: T.piqi) =
  (* save the original piqi *)
  piqi.P#original_piqi <- Some (copy_obj piqi); (* shallow copy *)

  check_assign_module_name ?modname fname piqi;

  if cache then Piqi_db.add_piqi piqi;

  (* get unparsed fields before we load dependencies *)
  (* TODO, XXX: this function call is meaningless if Piqi is not parsed from Piq *)
  let unknown_fields = Piqobj_of_piq.get_unknown_fields () in

  (* load imports and includes *)
  load_dependecies piqi;

  (* get all unique included modules recursively; piqi will become the last
   * element of the list *)
  let modules = get_includes piqi in
  (* expand included_in to contain the list of all included modules including
   * the current one *)
  piqi.P#included_piqi <- modules;

  (* get all imports from this module and included modules *)
  let resolved_imports = get_resolved_imports modules in
  piqi.P#resolved_import <- resolved_imports;

  (* check for duplicates & looped imports including self-import loop *)
  check_imports piqi;

  let imported_defs = get_imported_defs resolved_imports in
  piqi.P#imported_piqdef <- imported_defs;

  (* fill idtable with their imported modules' definitions *)
  let idtable = Idtable.empty in
  let idtable = add_imported_piqdefs idtable imported_defs in

  (* boot defintions *)
  let boot_defs, boot_custom_fields =
    match !boot_piqi with
      | Some x when !Config.noboot = false ->
          (* NOTE: boot defs should be already extended *)
          x.P#resolved_piqdef, x.P#custom_field
      | _ ->
          (* boot module is being processed right now, or --noboot command-line
           * option was specified *)
          [], []
  in
  (* add defintions from the boot module to the idtable *)
  let idtable = add_piqdefs idtable boot_defs in

  (* get all definitions from all included modules and the current module *)
  let defs = get_piqdefs modules in

  (* get all extensions *)
  let extensions = get_extensions modules in

  (* NOTE: for local definitions we're considering custom fields defined only
   * in this module *)
  let custom_fields = piqi.P#custom_field @ boot_custom_fields in
  check_unknown_fields unknown_fields custom_fields;

  (* expand all extensions over all definitions *)
  (* NOTE, DOC: boot defs can not be extended *)
  let extended_defs =
    match extensions with
      | [] -> defs
      | _ ->
          (* defs should be correct before extending them *)
          (* XXX: can they become correct after extension while being
           * incorrect before? *)
          check_defs idtable defs;

          List.iter check_extension piqi.P#extend;

          (* NOTE: for extensions we're considering all custom fields from all
           * included modules *)
          let custom_fields = custom_fields @ (get_custom_fields modules) in
          expand_extensions defs extensions custom_fields
  in
  (* preserve the original defintions by making a copy *)
  let resolved_defs = copy_defs extended_defs in

  (* if the module includes (or is itself) piqi.org/piqi, use hash-based field
   * and option codes instead of auto-enumerated ones
   *
   * NOTE: code assignment is needed only for .piqi, Piqi specifications encoded
   * in other formats must already include explicitly speficied (and correct!)
   * wire codes.
   *
   * NOTE: this step must be performed before resolving defaults -- this is
   * critical for potential piqi extensions such as those used in various piqic
   *)
  if C.is_self_spec piqi then Piqi_wire.add_hashcodes resolved_defs;

  (* check defs, resolve defintion names to types, assign codes, resolve default
   * fields *)
  let idtable = resolve_defs ~piqi idtable resolved_defs in

  (* get definitions derived from function parameters *)
  let func_defs = !get_function_defs piqi in

  (* resolving them separately, because they should not be addressable from the
   * normal definitions and from other function definitions as well *)
  List.iter (fun x -> ignore (resolve_defs ~piqi idtable [x])) func_defs;

  (* now, combine the two together *)
  let resolved_defs = resolved_defs @ func_defs in

  piqi.P#extended_piqdef <- extended_defs;
  piqi.P#resolved_piqdef <- resolved_defs;

  (* run registered processing hooks *)
  List.iter (fun f -> f idtable piqi) !processing_hooks;

  (* resolve defaults ANY to OBJ using field types and codes; we need to do it
   * after executing hooks, because otherwise json names will be unresolved and
   * default field resolution will fail *)
  List.iter resolve_defaults resolved_defs;
  ()
 

(* XXX: disallow include and import of the same module or produce a warning? *)
(* NOTE: using absolute paths by this point *)
and load_piqi_file ?modname fname =
  trace "file: %s\n" fname;
  trace_enter ();
  (*
  Piqloc.trace := true;
  *)
  let ast = read_piqi_file fname in
  (* cache only those modules that were included/imported/referenced by their
   * modname *)
  let cache = modname <> None in
  let piqi = load_piqi_ast ?modname ~cache fname ast in
  trace_leave ();
  piqi


(* This function is not used at the time
and load_piqi_channel ?modname fname ch =
  let ast = read_piqi_channel fname ch in
  load_piqi_ast ?modname fname ast
*)


and load_piqi_string fname content =
  let ast = read_piqi_string fname content in
  load_piqi_ast fname ast ~cache:false


and load_piqi_ast ?modname ?(cache=true) fname (ast :T.ast) =
  let piqi = parse_piqi ast in
  process_piqi ?modname ~cache ~fname piqi;
  piqi


and load_piqi_module modname =
  check_modname modname;
  (* check if the module is already loaded *)
  try Piqi_db.find_piqi modname
  with Not_found ->
    let fname =
      try
        Piqi_file.find_piqi modname
      with
        Not_found ->
          error modname ("piqi module is not found: " ^ quote modname)
    in
    let piqi = load_piqi_file fname ~modname in
    piqi


and load_dependecies (piqi:T.piqi) =

  let included_piqi = load_includes piqi.P#includ in
  piqi.P#included_piqi <- included_piqi;

  (* preserve the original imports *)
  let resolved_imports = copy_imports piqi.P#import in
  load_imports resolved_imports;
  piqi.P#resolved_import <- resolved_imports;
  ()


and load_imports l = List.iter load_import l

and load_import x =
  let open Import in (
    trace "import: %s\n" x.Import.modname;
    (* load imported module *)
    let piqi = load_piqi_module x.modname in
    (* save imported piqi in import.piqi field *)
    x.piqi <- Some piqi;
    assign_import_name x;
  )


and load_includes l = List.map load_include l

and load_include x =
  let open Includ in (
    trace "include: %s\n" x.Includ.modname;
    (* load included piqi module if it isn't already *)
    let piqi = load_piqi_module x.modname in
    piqi
  )


(* XXX: is it correct in case of piqicc and piqic? *)
let embedded_modname = "embedded/piqi.org/piqi-lang"


let find_embedded_piqtype name =
  Piqi_db.find_piqtype (embedded_modname ^ "/" ^ name)


(*
 * booting code; preparing the initial statically defined piqi defintions
 *)
let boot () =
  trace "boot(0)\n";
  (* process embedded Piqi self-specification *)
  (* don't cache them as we are adding the spec to the DB explicitly below *)
  process_piqi T.boot_piqi ~cache:false;
  boot_piqi := Some T.boot_piqi;
  process_piqi T.piqi_lang ~cache:false;
  process_piqi T.piqi_spec ~cache:false;

  (* add the boot spec to the DB under a special name *)
  T.boot_piqi.P#modname <- Some "embedded/piqi.org/piqi-boot";
  Piqi_db.add_piqi T.boot_piqi;

  (* add the self-spec to the DB under a special name *)
  T.piqi_spec.P#modname <- Some "embedded/piqi.org/piqi";
  Piqi_db.add_piqi T.piqi_spec;

  (* add the self-spec to the DB under a special name *)
  T.piqi_lang.P#modname <- Some embedded_modname;
  Piqi_db.add_piqi T.piqi_lang;

  (* resolved type definition for the Piqi language *)
  piqi_lang_def := find_embedded_piqtype "piqi";
  (* resolved type definition for the Piqi specification *)
  piqi_spec_def := Piqi_db.find_piqtype "embedded/piqi.org/piqi/piqi";
  (* resolved "piqdef" type definition *)
  piqdef_def := find_embedded_piqtype "piqdef";

  (* turn boot mode off *)
  boot_mode := false;

  (* reset wire location counters *)
  Piqloc.icount := 0;
  Piqloc.ocount := 0;

  (* initialize Piqi loader; doing it this way, because Piqi and Piqi_db are
   * mutually recursive modules *)
  Piqi_db.piqi_loader := Some load_piqi_file;

  trace "boot(1)\n";
  ()


let _ =
  boot ()


let load_embedded_boot_module (modname, content) =
  trace "loading embedded module: %s\n" modname;
  trace_enter ();
  let fname = "embedded/" ^ modname in
  let piqi = load_piqi_string fname content in
  piqi.P#modname <- Some modname; (* fix the modname *)
  Piqi_db.add_piqi piqi;
  trace_leave ();
  piqi


let load_embedded_boot_modules () =
  let rec aux = function
    | [x] ->
        (* the last entry in the list is the boot module; return it after
         * processing *)
        load_embedded_boot_module x
    | h::t -> 
        ignore (load_embedded_boot_module h);
        aux t
    | _ ->
        assert false
  in
  let boot_piqi = aux !T.embedded_piqi in
  (* make loaded modules unsearcheable after loading all of them *)
  List.iter (fun (modname, _) -> Piqi_db.remove_piqi modname) !T.embedded_piqi;
  boot_piqi


let load_embedded_boot_piqi () =
  (* Overriding already loaded boot_piqi with exactly the same boot module but
   * now it has correct location info because it is parsed from string
   * representation *)
  (* Not running piqicc and haven't loaded a custom module before: *)
  if C.is_boot_piqi T.boot_piqi && !T.embedded_piqi <> []
  then
    boot_piqi :=
      (
        (* reset previous boot module *)
        boot_piqi := None;
        trace "boot using embedded modules\n";
        trace_enter ();
        (* XXX: error handling *)
        let piqi = load_embedded_boot_modules () in
        trace_leave ();
        Some piqi
      )


(* used only by piqicc to load a custom Piqi boot module from a file *)
let load_boot_piqi boot_file  =
  boot_piqi :=
    (
      (* reset previous boot module *)
      boot_piqi := None;
      trace "boot using boot file: %s\n" boot_file;
      trace_enter ();
      (* TODO: error handling *)
      let piqi = load_piqi_file boot_file in
      trace_leave ();
      Some piqi
    )


(* this is a local function; it can be called more than once, but produce an
 * effect only on its first run *)
let init () =
  if !Config.debug_level > 0 || !Config.flag_trace
  then load_embedded_boot_piqi ()


(* public interface: read piqi file *)
let read_piqi fname :T.ast =
  let ch = Piqi_main.open_input fname in
  read_piqi_channel fname ch


(* public interface: load piqi file *)
let load_piqi fname :T.piqi =
  init ();
  trace "loading piqi file: %s\n" fname;
  trace_enter ();
  let ast = read_piqi fname in
  let piqi = load_piqi_ast fname ast ~cache:true in
  trace_leave ();
  piqi


(*
 * this code was previously used by piqicc; keeping it here for a while just
 * in case:

let convert_obj new_piqtype obj =
  debug "convert_obj(0)\n";
  trace_enter ();
  (* XXX *)
  C.resolve_defaults := false;
  (* serialize to ast and read back as a differnt piqtype *)
  let ast = Piqobj_to_piq.gen_obj obj in

  (* XXX: setting this option in order to delay, and then ignore all parsing
   * warnings *)
  Piqobj_of_piq.delay_unknown_warnings := true;

  let res = Piqobj_of_piq.parse_obj new_piqtype ast in

  (* reset all warnings *)
  ignore (Piqobj_of_piq.get_unknown_fields ());

  trace_leave ();
  res


(* XXX: this can be implemented more efficiently using table-base precalculated
 * mapping *)
let convert_binobj piqtype new_piqtype binobj =
  debug "convert_binobj(0)\n";
  trace_enter ();
  let obj = Piqobj_of_wire.parse_binobj piqtype binobj in
  debug_loc "convert_binobj(1)";
  let new_obj = convert_obj new_piqtype obj in
  debug_loc "convert_binobj(2)";
  let res = Piqobj_to_wire.gen_binobj new_obj in
  Piqloc.icount := !Piqloc.ocount;
  debug_loc "convert_binobj(3)";
  trace_leave ();
  res
*)

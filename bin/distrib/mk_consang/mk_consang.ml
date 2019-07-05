(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

let fname = ref ""
let indexes = ref false
let scratch = ref false
let verbosity = ref 2
let tlim = ref (-1)
let fast = ref false

let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"
let speclist =
  [("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
   ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
   ("-fast", Arg.Set fast, " faster, but use more memory");
   "-i", Arg.Set indexes, ": build the indexes again";
   "-t", Arg.Int (fun i -> tlim := i), " <int>: time limit in seconds";
   "-scratch", Arg.Set scratch, ": from scratch";
   "-mem", Arg.Set Outbase.save_mem,
   ": Save memory, but slower when rewritting database";
   "-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database."]
let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

type ('index, 'item) field_info =
  { fi_nb : int;
    fi_ht : ('index, 'item) Hashtbl.t;
    fi_index_of_int : int -> 'index;
    fi_dir : string }

#ifdef GWDB1
let simple_output bname base carray =
  match carray with
  | Some _tab -> assert false
  | None ->
    Gwdb1.apply_base1 base
      (fun base ->
         let bname = base.Dbdisk.data.Dbdisk.bdir in
         let no_patches =
           not (Sys.file_exists (Filename.concat bname "patches"))
         in
         Outbase.gen_output (no_patches && not !indexes) bname base);
    (* On recalcul le nombre reel de personnes. *)
    Util.init_cache_info bname (Gwdb1.ToGwdb.base base)
#endif


let main () =
  Argl.parse speclist anonfun errmsg;
  if !fname = "" then
    begin
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
      exit 2
    end;
  if !verbosity = 0 then Mutil.verbose := false ;
  Secure.set_base_dir (Filename.dirname !fname);
  Lock.control_retry
    (Mutil.lock_file !fname)
    ~onerror:Lock.print_error_and_exit
    (fun () ->
       let base = Gwdb.open_base !fname in
       if !fast then begin
         Gwdb.load_persons_array base;
         Gwdb.load_families_array base;
         Gwdb.load_ascends_array base;
         Gwdb.load_unions_array base;
         Gwdb.load_couples_array base;
         Gwdb.load_descends_array base;
         Gwdb.load_strings_array base
       end ;
       try
         Sys.catch_break true;
         let carray = ConsangAll.compute ~verbosity:!verbosity base !tlim !scratch in
         simple_output !fname (Gwdb1.OfGwdb.base base) carray
       with Consang.TopologicalSortError p ->
         Printf.printf "\nError: loop in database, %s is his/her own ancestor.\n"
           (Gutil.designation base p);
         flush stdout;
         exit 2)

let _ = Printexc.print main ()

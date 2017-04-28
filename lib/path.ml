(* Some global path, independant from base used. *)
let etc = ref "etc"
let lang = ref "lang"
let cnt = ref "cnt"
(* all three paths are reset by the -hd parameter (if present) at start time *)
let sharelib =
  Array.fold_left Filename.concat
    (try Sys.getenv "GWPREFIX" with Not_found -> "/usr") (* FIXME *)
    [| "share" ; "geneweb" |]
let direct = ref false

(* Lazy so that we can set [cnt] before actually accessing the file. *)
let gwd_lock_file = lazy (Filename.concat !cnt "gwd.lck")

type t =
  { dir_documents : string
  ; dir_portraits : string
  ; dir_portraits_bak : string
  ; file_conf : string
  ; dir_notes : string
  ; dir_wiznotes : string
  ; dir_root : string
  ; dir_images : string
  ; dir_password : string
  ; dir_bases : string
  ; dir_binaries : string
  ; dir_cnt : string
  ; dir_history : string
  ; file_ts : string
  ; file_ts_visitor : string
  ; file_restrict : string
  ; file_synchro_patches : string
  ; file_cmd : string
  ; file_base : string
  ; file_particles : string
  ; file_base_acc : string
  ; file_strings_inx : string
  ; file_names_inx : string
  ; file_names_acc : string
  ; file_patches : string
  ; file_notes_aliases : string
  ; file_forum : string
  ; file_history : string
  ; file_notes_links : string
  ; file_snames_dat : string
  ; file_snames_inx : string
  ; file_fnames_dat : string
  ; file_fnames_inx : string
  ; file_wizard_log: string
  ; file_friend_log : string
  ; file_counts : string
  ; file_lock : string
  ; file_update_log : string
  ; file_cache_info : string
  }

(* some paths are relative to the current base dir (full_bdir) *)

let path_from_bname s =
  let bdir =
    if Filename.check_suffix s ".gwb" then s
    else if Filename.check_suffix s ".gwb/" then Filename.chop_suffix s "/"
    else s ^ ".gwb"
  in
  let dir_bases = Secure.base_dir () in (* -bd argument *)
  let dir_binaries = Filename.current_dir_name in
  let full_bdir = Filename.concat dir_bases bdir in 
  let dir_cnt = List.fold_left Filename.concat full_bdir [ "etc" ; "cnt" ] in
  { dir_portraits = List.fold_left Filename.concat full_bdir [ "documents" ; "portraits" ]
  ; dir_documents = Filename.concat full_bdir "documents"
  ; file_conf = List.fold_left Filename.concat full_bdir [ "etc" ; "config.txt" ]
  ; dir_notes = Filename.concat bdir "notes"
  ; dir_wiznotes = Filename.concat bdir "wiznotes"
  ; dir_root = full_bdir
  ; dir_images = List.fold_left Filename.concat full_bdir [ "documents" ; "images" ]
  ; dir_portraits_bak = List.fold_left Filename.concat full_bdir [ "documents" ; "portraits" ; "saved" ]
  ; dir_password = dir_bases
  ; dir_bases = dir_bases
  ; dir_binaries = dir_binaries
  ; dir_cnt
  ; dir_history = Filename.concat full_bdir "history"
  ; file_ts = Filename.concat bdir "tstab"
  ; file_ts_visitor = Filename.concat bdir "tstab_visitor"
  ; file_snames_dat = Filename.concat bdir "snames.dat"
  ; file_snames_inx = Filename.concat bdir "snames.inx"
  ; file_fnames_dat = Filename.concat bdir "fnames.dat"
  ; file_fnames_inx = Filename.concat bdir "fnames.inx"
  ; file_restrict = Filename.concat bdir "restrict"
  ; file_synchro_patches = Filename.concat bdir "synchro_patches"
  ; file_cmd = Filename.concat full_bdir "command.txt"
  ; file_base = Filename.concat bdir "base"
  ; file_particles = Filename.concat bdir "particles.txt"
  ; file_base_acc = Filename.concat bdir "base.acc"
  ; file_strings_inx = Filename.concat bdir "strings.inx"
  ; file_names_inx = Filename.concat bdir "names.inx"
  ; file_names_acc = Filename.concat bdir "names.acc"
  ; file_patches = Filename.concat bdir "patches"
  ; file_notes_aliases = Filename.concat full_bdir "notes.alias"
  ; file_forum = Filename.concat full_bdir "forum"
  ; file_history = List.fold_left Filename.concat full_bdir [ "history" ; "history.txt" ]
  ; file_notes_links = Filename.concat full_bdir "notes_links"
  ; file_wizard_log = Filename.concat dir_cnt "wizard.log"
  ; file_friend_log = Filename.concat dir_cnt "friends.log"
  ; file_counts = Filename.concat dir_cnt "counts.txt"
  ; file_lock = Filename.concat dir_cnt "lock"
  ; file_update_log = Filename.concat dir_cnt "update.log"
  ; file_cache_info = Filename.concat bdir "cache_info"
  }

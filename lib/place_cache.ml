(* Copyright (c) 1998-2007 INRIA *)
(* place_cache.ml — cache permanent de l'index PPS + utilitaires *)

let src = Logs.Src.create ~doc:"PlaceCache" "PLCC"

module Log = (val Logs.src_log src : Logs.LOG)
module Driver = Geneweb_db.Driver

(* ------------------------------------------------------------------ *)
(* Flags : combinaison de critères actifs                              *)
(* ------------------------------------------------------------------ *)

type flags = {
  add_birth : bool;
  add_baptism : bool;
  add_death : bool;
  add_burial : bool;
  add_marriage : bool;
  inverted : bool;
}

let flags_suffix f =
  (if f.add_birth then "bi" else "")
  ^ (if f.add_baptism then "ba" else "")
  ^ (if f.add_death then "de" else "")
  ^ (if f.add_burial then "bu" else "")
  ^ (if f.add_marriage then "ma" else "")
  ^ if f.inverted then "v" else ""

(* ------------------------------------------------------------------ *)
(* Chemin du fichier cache                                             *)
(* ------------------------------------------------------------------ *)

(** [cache_path bdir flags] retourne le chemin complet du fichier cache pour une
    combinaison de flags donnée. Exemple : "<base>.gwb/pps_bide.plc" pour bi=on,
    de=on. *)
let cache_path bdir flags =
  Filename.concat bdir ("pps_" ^ flags_suffix flags ^ ".plc")

(* ------------------------------------------------------------------ *)
(* Validité du cache                                                   *)
(* ------------------------------------------------------------------ *)

(** Le cache est valide si son mtime est >= au mtime du fichier de référence de
    la base. On utilise la même logique que [Driver.date_of_last_change] : on
    préfère "patches" à "base" car c'est lui qui change à chaque modification
    web.

    Si le fichier cache n'existe pas, ou si une erreur Unix survient, on
    retourne [false] pour forcer un rebuild. *)
let cache_is_valid bdir cache_path =
  try
    let ref_file =
      let p = Filename.concat bdir "patches" in
      if Sys.file_exists p then p else Filename.concat bdir "base"
    in
    let ref_mtime = (Unix.stat ref_file).Unix.st_mtime in
    let cache_mtime = (Unix.stat cache_path).Unix.st_mtime in
    cache_mtime >= ref_mtime
  with Unix.Unix_error _ -> false

(* ------------------------------------------------------------------ *)
(* Type des entrées de cache                                           *)
(* ------------------------------------------------------------------ *)

type entry = (string list * string) * (string * Driver.iper list) list
(** Une entrée de l'index PPS :
    - clé : (liste de composants de lieu * suburb)
    - valeur: liste de (nom de famille * liste d'ipers) *)

let dummy_entry : entry = (([], ""), [])

(* ------------------------------------------------------------------ *)
(* Sérialisation / désérialisation                                     *)
(* ------------------------------------------------------------------ *)

let magic = "GwPl0001"

(** Écrit le tableau [arr] dans [path] de façon atomique (via un fichier
    temporaire) pour éviter tout fichier cache corrompu. *)
let write_cache path (arr : entry array) =
  let tmp = path ^ ".tmp" in
  try
    let oc = open_out_bin tmp in
    try
      output_string oc magic;
      Marshal.to_channel oc arr [];
      close_out oc;
      (try Sys.remove path with Sys_error _ -> ());
      Sys.rename tmp path;
      Log.info (fun k ->
          k "PPS cache written: %s (%d entries)" path (Array.length arr))
    with e ->
      close_out_noerr oc;
      raise e
  with e ->
    (try Sys.remove tmp with Sys_error _ -> ());
    Log.warn (fun k ->
        k "PPS cache write failed %s: %s" path (Printexc.to_string e))

(** Lit le cache depuis [path]. Retourne [None] en cas d'erreur ou de magic
    number incorrect (cache obsolète d'une version antérieure). *)
let read_cache path : entry array option =
  try
    let ic = open_in_bin path in
    let result =
      try
        let ok = Mutil.check_magic magic ic in
        if ok then
          let arr : entry array = Marshal.from_channel ic in
          Some arr
        else (
          Log.warn (fun k -> k "PPS cache magic mismatch: %s" path);
          None)
      with e ->
        Log.warn (fun k ->
            k "PPS cache read error %s: %s" path (Printexc.to_string e));
        None
    in
    close_in_noerr ic;
    result
  with Sys_error _ -> None

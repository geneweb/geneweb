(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Gwcomp

type t =
  { before : unit -> unit
  ; onread : string -> string -> bool -> int -> unit
  ; onclose : in_channel -> unit
  }

let check_magic fname ic =
  let b = really_input_string ic (String.length magic_gwo) in
  if b <> magic_gwo then
    if String.sub magic_gwo 0 4 = String.sub b 0 4 then
      failwith
        ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
    else
      failwith
        ("\"" ^ fname ^
         "\" is not a GeneWeb object file, or it is a very old version")

let next_family_fun_templ f gwo_list =
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not !(Mutil.verbose) then fun () -> ()
    else if ngwo < 60 then fun () -> Printf.eprintf "."; flush stderr
    else
      let bar_cnt = ref 0 in
      let run () = ProgrBar.run !bar_cnt ngwo; incr bar_cnt in
      ProgrBar.empty := 'o'; ProgrBar.full := '*'; ProgrBar.start (); run
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  f.before () ;
  fun () ->
    let rec loop () =
      let r =
        match !ic_opt with
        | None -> None
        | Some ic ->
          match try Some (input_value ic : gw_syntax) with End_of_file -> None with
          | Some fam -> Some fam
          | None -> f.onclose ic ; close_in ic ; ic_opt := None ; None
      in
      match r with
      | Some fam -> Some fam
      | None ->
        match !gwo_list with
        | (x, separate, shift) :: rest ->
          run ();
          gwo_list := rest;
          let ic = open_in_bin x in
          check_magic x ic;
          let v = input_value ic in
          f.onread v x separate shift ;
          ic_opt := Some ic ;
          loop ()
        | [] ->
          if ngwo < 10 || not !(Mutil.verbose) then ()
          else if ngwo < 60 then begin Printf.eprintf "\n"; flush stderr end
          else ProgrBar.finish () ;
          None
    in
    loop ()


let just_comp = ref false

let separate = ref false
let shift = ref 0
let files = ref []

let speclist =
  [ ("-c", Arg.Set just_comp, "Only compiling")
  ; ("-sep", Arg.Set separate, " Separate all persons in next file")
  ; ("-sh", Arg.Int (fun x -> shift := x), "<int> Shift all persons numbers in next files")
  ; ("-version", Arg.Unit Util.print_version_commit, " print version and commit numbers")
  ]

let anonfun x =
  let sep = !separate in
  if Filename.check_suffix x ".gw" then ()
  else if Filename.check_suffix x ".gwo" then ()
  else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
  separate := false;
  files := (x, sep, !shift) :: !files

let errmsg =
  "Usage: gwc [options] [files]\n\
   where [files] are a list of files:\n  \
   source files end with .gw\n  \
   object files end with .gwo\n\
   and [options] are:"

let main spec =
  Mutil.verbose := false ;
  Argl.parse (spec @ speclist) anonfun errmsg ;
  let gwo =
    List.rev_map
      (fun (x, separate, shift) ->
         if Filename.check_suffix x ".gw" then begin
           begin try Gwcomp.comp_families x
             with e -> Printf.printf "File \"%s\", line %d:\n" x !line_cnt ; raise e
           end ;
           (x ^ "o", separate, shift)
         end else (x, separate, shift) )
      (List.rev !files)
  in
  if ! just_comp then exit 0 ;
  let f : type a . (a -> t) -> ((a -> unit -> Gwcomp.gw_syntax option) -> bool) -> (bool) = fun f init ->
    let next_family_fun (fi : a) = next_family_fun_templ (f fi) gwo in
    init next_family_fun
  in
  f

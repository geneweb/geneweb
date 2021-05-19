open Geneweb.Config

module Util = Geneweb.Util
module Hutil = Geneweb.Hutil
module ImageDisplay = Geneweb.ImageDisplay
module SrcfileDisplay = Geneweb.SrcfileDisplay
module Output = Geneweb.Output

(* removed Filename.basename from those two functions *)

let new_source_file_name conf fname =
  let bname = conf.bname in
  let lang = conf.lang in
  let fname1 =
    List.fold_right Filename.concat [Util.base_path ["src"] bname; lang]
      (fname ^ ".txt")
  in
  if Sys.file_exists fname1 then fname1
  else
    Filename.concat (Util.base_path ["src"] bname)
      (fname ^ ".txt")

let new_print_source_image conf f =
  let fname =
    if f.[0] = '/' then String.sub f 1 (String.length f - 1) else f
  in
    let fname = Util.source_image_file_name conf.bname fname in
    if ImageDisplay.print_image_file conf fname then () else Hutil.incorrect_request conf

let new_print_source conf base fname =
  let channel =
    try Some (Secure.open_in (new_source_file_name conf fname)) with
      Sys_error _ -> None
  in
  match channel with
    Some ic ->
      let title _ = Output.print_string conf fname in
      Hutil.header_without_page_title conf title;
      SrcfileDisplay.copy_from_stream conf base
        (Stream.of_channel ic) SrcfileDisplay.Source;
      Hutil.gen_trailer true conf
  | _ ->
      let title _ = Output.printf conf "Error" in
      Hutil.header conf title;
      Output.printf conf "<ul><li>Cannot access file \"%s.txt\"</ul>" fname;
      Hutil.gen_trailer true conf;
      raise Exit


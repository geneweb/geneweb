(* camlp4r *)

open Def;
open Gwdb;
open Printf;


value convert_dico_place file =
  match try Some (open_in fname) with [ Sys_error _ -> None] with
  [ Some ic ->
      do {
        let ht = Hashtbl.create 501 in
        let list =
          loop [] where rec loop list = 
            match try Some (input_line ic) with [End_of_file -> None] with
            [ Some s ->
                if s = "" then loop env
                else loop [s :: env]
            | None -> List.rev env ]
        in
        close_in ic;
        list
      }
  | None -> [] ]
;

value place_files = ref [];

value speclist =
   [("-file",
     Arg.String (fun x -> place_files.val := [x :: place_files.val]),
     "<file>\n       Convert the file into a GeneWeb place dictionary")]
;
value anonfun _ = do { Arg.usage speclist usage_msg; exit 2 };
value usage = "Usage: gwCreateDicoPlace [-file <file>]";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if place_files.val = [] then do { Arg.usage speclist usage; exit 2; }
    else ();
    List.iter convert_dico_place place_files.val;
  }
;

main ();

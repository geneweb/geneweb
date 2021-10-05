open Geneweb
open Config
open Gwdb
open Plugin_gedcom_import_lib

(* FIXME: factorize with plugin_fixbase.ml *)
module UI = struct

  let enabled conf s = List.assoc_opt s conf.env = Some "on"

  let print_arg conf (name, kind, doc) =
    match kind with
    | `Arg_Set ->
      Output.print_string conf {|<p><label><input type="checkbox" name="|} ;
      Output.print_string conf name ;
      Output.print_string conf {|" value="on"> |} ;
      Output.print_string conf doc ;
      Output.print_string conf {|</label></p>|} ;
    | _ -> assert false

  let form ?(enc = "application/x-www-form-urlencoded") conf meth m submit bef args aft =
    Output.print_string conf {|<form action="|} ;
    Output.print_string conf conf.command ;
    Output.print_string conf {|" method="|} ;
    Output.print_string conf meth ;
    Output.print_string conf {|" enctype="|} ;
    Output.print_string conf enc ;
    Output.print_string conf {|">|} ;
    Output.print_string conf {|<input type="hidden" name="m" value="|} ;
    Output.print_string conf m ;
    Output.print_string conf {|">|} ;
    bef () ;
    List.iter (print_arg conf) args ;
    aft () ;
    Output.print_string conf {|<input type="submit" value="|} ;
    Output.print_string conf submit ;
    Output.print_string conf {|">|} ;
    Output.print_string conf {|</form>|}

end

let gedcom_import conf _base =
  let title = Util.transl conf "plugin_gedcom_import_GEDCOM_IMPORT" in
  !GWPARAM.wrap_output conf title @@ fun () ->
  UI.form ~enc:"multipart/form-data" conf "POST" "GEDCOM_IMPORT_OK" (Util.transl conf "plugin_gedcom_import_submit")
    begin fun () ->
      Output.print_string conf {|<input type="file" name="ged">|} ;
    end
    []
    begin fun () ->
      ()
    end

let parse_req r =
  let rec loop acc i =
    let j = String.index_from r i '\010' in
    let s = String.sub r i (if String.get r (j - 1) = '\013' then j - i - 1 else j - i) in
    if s = ""
    then ( List.rev acc
         , if j + 1 >= String.length r then "" else String.sub r (j + 1) (String.length r - j - 1)
         )
    else loop (s :: acc) (j + 1)
  in loop [] 0

let gedcom_import_ok conf _base =
  let title = Util.transl conf "plugin_gedcom_import_GEDCOM_IMPORT_OK" in
  !GWPARAM.wrap_output conf title @@ fun () ->
  let h, ged = List.assoc "ged" conf.env |> parse_req in
  print_endline __LOC__ ;
  print_string ged ;
  print_endline __LOC__ ;
  List.iter (fun s -> Output.print_string conf @@ "<p>" ^ s ^ "</p>") h ;
  let ni, nf = Gedcom_import.length ged in
  Output.print_string conf @@ "<p>nb ind: " ^ string_of_int ni ^ "</p>" ;
  Output.print_string conf @@ "<p>nb fam: " ^ string_of_int nf ^ "</p>" ;
  let nodes = Gedcom_import.nodes ged in
  let rec print n =
    Output.print_string conf @@ "<div style=\"margin-left:1em;\">" ;
    Output.print_string conf @@ Gedcom.tag @@ Gedcom.node n ;
    List.iter print (Gedcom.children n) ;
    Output.print_string conf @@ "</div>" ;
  in
  List.iter print nodes ;
  Output.print_string conf {|<code><pre>|} ;
  Output.print_string conf ged ;
  Output.print_string conf {|</pre></code>|} ;
  List.iter (fun s -> Output.print_string conf "<p>" ; Output.print_string conf s ; Output.print_string conf "</p>") conf.request

let ns = "gedcom_import"

let opt_password =
  match Sys.getenv_opt "GW_PLUGIN_GEDCOM_IMPORT_PASSWORD" with
  | Some "" | None -> None
  | x -> x

let opt_manitou =
  match Sys.getenv_opt "GW_PLUGIN_GEDCOM_IMPORT_ONLY_MANITOU" with
  | Some ("on"|"ON"|"y"|"Y"|"1") -> true
  | _ -> false

let _ =
  let aux fn _assets conf = function
    | Some base ->
      if if opt_manitou then conf.manitou else conf.wizard
      then
        if opt_password = List.assoc_opt "password" conf.env
        then (fn conf base ; true)
        else false
      else false
    | None -> false
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "GEDCOM_IMPORT", aux gedcom_import
    ; "GEDCOM_IMPORT_OK", aux gedcom_import_ok
    ]

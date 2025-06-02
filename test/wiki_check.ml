module Html = Tyxml.Html
module Ast = Geneweb_tlsw.Ast
module Parser = Geneweb_tlsw.Parser
module Loc = Geneweb_tlsw.Loc
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Compat = Geneweb_compat

let ( // ) = Filename.concat
let pp_header ppf () = Fmt.pf ppf "Syntax error:"

type mode = Incremental | Batch

let with_timer f =
  let start = Unix.gettimeofday () in
  let r = f () in
  let stop = Unix.gettimeofday () in
  (r, stop -. start)

let with_bar ~total =
  let bar ~total =
    Progress.Line.(
      list [ spinner (); bar total; count_to total; percentage_of total ])
  in
  Progress.with_reporter (bar ~total)

let iter_notes base ipers f =
  Collection.iteri
    (fun _i iper ->
      let p = Driver.poi base iper in
      let s = Driver.get_notes p |> Driver.sou base in
      f p s)
    ipers

let process_base ~mode ~in_memory bname =
  if in_memory then Driver.load_database (Secure.base_dir () // bname);
  Driver.with_database (Secure.base_dir () // bname) @@ fun base ->
  let ipers = Driver.ipers base in
  let total = Driver.nb_of_persons base in
  let on_err p ~loc err =
    let fn = Driver.get_first_name p |> Driver.sou base in
    let sn = Driver.get_surname p |> Driver.sou base in
    let occ = Driver.get_occ p in
    Logs.err (fun k ->
        k "%a in notes of (%s, %s, %d): %s@ %a@."
          (Fmt.styled (`Fg `Red) pp_header)
          () fn sn occ err Loc.pp_with_input loc);
    match mode with
    | Batch -> ()
    | Incremental ->
        Logs.app (fun k -> k "Press enter to continue...@.");
        ignore (read_line () : string)
  in
  let parse p s = ignore (Parser.parse ~on_err:(on_err p) s : Ast.t list) in
  match mode with
  | Batch ->
      with_bar ~total @@ fun f ->
      iter_notes base ipers @@ fun p s ->
      parse p s;
      f 1
  | Incremental -> iter_notes base ipers @@ parse

let process_gw fl =
  Compat.In_channel.with_open_text fl @@ fun ic ->
  let s = Compat.In_channel.input_all ic in
  let on_err ~loc err =
    Logs.err (fun k ->
        k "%a: %s@ %a@."
          (Fmt.styled (`Fg `Red) pp_header)
          () err Loc.pp_with_input loc)
  in
  ignore (Parser.parse ~on_err s : Ast.t list)

let run ~bd ~mode ~in_memory bases gws =
  Secure.set_base_dir bd;
  let (), duration =
    with_timer @@ fun () ->
    List.iter (fun bname -> process_base ~mode ~in_memory bname) bases;
    List.iter (fun fl -> process_gw fl) gws
  in
  Logs.app (fun k -> k "Total time: %6.2fs" duration)

module Cmd = struct
  open Cmdliner
  open Cmdliner.Term.Syntax

  let mode = Arg.enum [ ("incremental", Incremental); ("batch", Batch) ]

  let bd =
    let doc = "Base directory" in
    Arg.(value & opt string "." & info [ "bd" ] ~docv:"PATH" ~doc)

  let bases =
    let doc = "Base name" in
    Arg.(value & opt_all string [] & info [ "b"; "base" ] ~docv:"BASE" ~doc)

  let gws =
    let doc = "Geneweb file" in
    Arg.(value & opt_all string [] & info [ "g"; "gw" ] ~docv:"FILE" ~doc)

  let in_memory =
    let doc = "Load data in memory" in
    Arg.(value & flag & info [ "in-memory" ] ~doc)

  let mode =
    let doc = "Mode" in
    Arg.(value & opt mode Batch & info [ "m"; "mode" ] ~docv:"MODE" ~doc)

  let t =
    let doc = "Check wiki syntax of databases" in
    Cmd.v (Cmd.info "Wiki_check" ~doc)
    @@ let+ bd = bd
       and+ mode = mode
       and+ in_memory = in_memory
       and+ bases = bases
       and+ gws = gws in
       run ~bd ~mode ~in_memory bases gws
end

let () =
  Fmt.set_style_renderer Format.err_formatter `Ansi_tty;
  let pp_header _ _ = () in
  Logs.set_reporter (Progress.logs_reporter ~pp_header ());
  exit @@ Cmdliner.Cmd.eval Cmd.t

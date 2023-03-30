
open Geneweb
open Config

let output_conf : Geneweb.Config.output_conf = (*{ status = assert false; header = assert false; body = assert false; flush = assert false}*) {
  status = (fun _ -> ());
  header = print_endline;
  body = print_endline;
  flush = (fun _ -> ());
}

let config = {Geneweb.Config.empty with output_conf; lang = "fr"; bname = "lalala"}

let init_assets () =
  GWPARAM.init := begin fun () ->
    let dir = List.fold_left (fun acc s -> Filename.concat acc s) "" ["..";"..";"..";"hd"] in
    print_endline dir;
    print_endline @@ Unix.getcwd ();
    Secure.add_assets dir
  end;
  !GWPARAM.init ()

let w_wizard fn conf bfile = fn conf bfile
let w_person fn conf base =
  let p = assert false in
  fn conf base p
let w_base base fn conf _ = fn conf base
let w_lock fn conf bfile = fn conf bfile
let incorrect_request conf = ()

let test conf base =
  let bfile = match conf.bname with "" -> None | b -> Some b in
  let m = Option.value ~default:"" (Geneweb.Util.p_getenv conf.env "m") in
  let w_base = w_base base in
  Gwd_lib.Request.mode_handler
    ~w_wizard
    ~w_person
    ~w_base
    ~w_lock
    ~incorrect_request
    ~conf
    ~bfile
    ~m

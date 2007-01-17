(* camlp4r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.1 2007-01-17 13:40:45 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Templ;

value error_cannot_access conf fname = do {
  let title _ = Wserver.wprint "Error" in
  Util.header conf title;
  tag "ul" begin
    tag "li" begin
      Wserver.wprint "Cannot access file \"%s.txt\".\n"
        fname;
    end;
  end;
  Util.trailer conf;
};

value interp conf base fname ifun env ep = do {
  let v = template_file.val in
  template_file.val := fname;
  try
    match input_templ conf fname with
    [ Some astl -> do {
        Util.html conf;
        Util.nl ();
        interp_ast conf (Some base) ifun env ep astl
      }
    | None ->
        error_cannot_access conf fname ]
  with e ->
    do { template_file.val := v; raise e };
  template_file.val := v;
};

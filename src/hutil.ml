(* camlp4r ./pa_html.cmo *)
(* $Id: hutil.ml,v 5.2 2007-01-17 14:07:00 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;

value gen_trailer with_logo conf =
  do {
    if not with_logo then ()
    else
      Wserver.wprint "\
<div>
<a href=\"%s\"><img src=\"%s/gwlogo.png\"
 alt=\"...\" width=\"64\" height=\"72\" style=\"border:0;float:%s\"%s></a>
<br%s>
</div>
" (Util.commd conf) (Util.image_prefix conf) conf.right
      conf.xhs conf.xhs;
    Templ.print_copyright conf;
    Util.include_hed_trl conf None ".trl";
    Wserver.wprint "</body>\n</html>\n";
  }
;

value trailer = gen_trailer True;

value incorrect_request conf = do {
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "incorrect request"))
  in
  Util.header conf title;
  Wserver.wprint "<p>\n";
  Util.print_link_to_welcome conf False;
  Wserver.wprint "</p>\n";
  trailer conf
};

value error_cannot_access conf fname = do {
  let title _ = Wserver.wprint "Error" in
  Util.header conf title;
  tag "ul" begin
    tag "li" begin
      Wserver.wprint "Cannot access file \"%s.txt\".\n"
        fname;
    end;
  end;
  trailer conf;
};

value interp conf base fname ifun env ep = do {
  let v = Templ.template_file.val in
  Templ.template_file.val := fname;
  try
    match Templ.input_templ conf fname with
    [ Some astl -> do {
        Util.html conf;
        Util.nl ();
        Templ.interp_ast conf (Some base) ifun env ep astl
      }
    | None ->
        error_cannot_access conf fname ]
  with e ->
    do { Templ.template_file.val := v; raise e };
  Templ.template_file.val := v;
};

(*
value incorrect_request : config -> unit;
value gen_trailer : bool -> config -> unit;
value trailer : config -> unit;
*)

(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_lock.ml,v 1.1.1.1 1998-09-01 14:32:05 ddr Exp $ *)

EXTEND
  Pcaml.expr: LEVEL "top"
    [ [ "lock"; fn = Pcaml.expr; "with";
        "["; UIDENT "Accept"; "->"; ea = Pcaml.expr;
        "|"; UIDENT "Refuse"; "->"; er = Pcaml.expr; "]" ->
          <:expr<
            if Lock.control $fn$ False (fun () -> $ea$) then ()
            else $er$ >>
      | "lock_wait"; fn = Pcaml.expr; "with";
        "["; UIDENT "Accept"; "->"; ea = Pcaml.expr;
        "|"; UIDENT "Refuse"; "->"; er = Pcaml.expr; "]" ->
          <:expr<
            if Lock.control $fn$ True (fun () -> $ea$) then () else $er$ >> ] ]
  ;
END;

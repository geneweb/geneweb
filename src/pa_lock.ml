(* camlp4r pa_extend.cmo q_MLast.cmo *)
(* $Id: pa_lock.ml,v 2.1 1999-03-08 11:19:01 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

EXTEND
  Pcaml.expr: LEVEL "top"
    [ [ "lock"; fn = Pcaml.expr; "with";
        "["; UIDENT "Accept"; "->"; ea = Pcaml.expr;
        "|"; UIDENT "Refuse"; "->"; er = Pcaml.expr; "]" ->
          <:expr<
            match Lock.control $fn$ False (fun () -> $ea$) with
            [ Some x -> x
            | None -> $er$ ] >>
      | "lock_wait"; fn = Pcaml.expr; "with";
        "["; UIDENT "Accept"; "->"; ea = Pcaml.expr;
        "|"; UIDENT "Refuse"; "->"; er = Pcaml.expr; "]" ->
          <:expr<
            match Lock.control $fn$ True (fun () -> $ea$) with
            [ Some x -> x
            | None -> $er$ ] >> ] ]
  ;
END;

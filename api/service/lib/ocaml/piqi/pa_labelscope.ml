(*pp camlp4orf *)
(* This code is in public domain. Written by Anton Lavrik.
 *
 * Tested with OCaml 3.12.0.
 *)

open Camlp4.PreCast
open Syntax
open Ast


let test_record_cons =
  Gram.Entry.of_parser "test_record_cons"
    (
      fun strm ->
        match Stream.npeek 5 strm with 
            [(UIDENT _,_); (KEYWORD "#",_); (KEYWORD "{",_); _; _] -> ()
          | [(UIDENT _,_); (KEYWORD ".",_); (UIDENT _,_); (KEYWORD "#",_); (KEYWORD "{",_)] -> ()
          | _ -> raise Stream.Failure
    )


EXTEND Gram
  GLOBAL: expr label_longident val_longident a_UIDENT a_LIDENT;

  expr: LEVEL "simple" [
    [
      test_record_cons; i = module_longident; "#"; e = expr LEVEL "simple" ->
        <:expr< $id:i$ . ( $e$ ) >>
    ]
  ];

  val_longident: [
    [
      m = a_UIDENT; "#"; f = a_LIDENT -> <:ident< $uid:m$.$lid:f$ >>
    ]
  ];

  label_longident: [
    [
      m = a_UIDENT; "#"; f = a_LIDENT -> <:ident< $uid:m$.$lid:f$ >>
    ]
  ];


(* TODO: #-labels in record patterns *)

END

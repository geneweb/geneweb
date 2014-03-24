#!/bin/sh
#cd (*
exec ocaml camlp4r.cma $0
*) ".";
(* $Id: unicode2win1251.sh,v 4.1 2002-09-26 09:43:11 ddr Exp $ *)

value ic = stdin;
value oc = stdout;

try
  while True do {
    let c1 = input_char ic in
    let c2 = input_char ic in
    if c2 = Char.chr 0 then output_char oc c1
    else if c2 = Char.chr 4 then
      let n = Char.code c1 + 0xb0 in
      if n >= 0 && n < 256 then output_char oc (Char.chr n)
      else output_char oc '?'
    else output_char oc '?';
  }
with
[ End_of_file -> flush stdout ];

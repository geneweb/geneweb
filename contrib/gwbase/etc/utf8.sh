#!/bin/sh
#cd (*
exec ocaml camlp4r.cma $0
*) ".";
(* $Id: utf8.sh,v 1.2 2005-07-22 01:46:29 ddr Exp $ *)

for i = 0 to 127 do {
  print_char (Char.chr i);
};
print_endline "";

for i = 0b11000000 to 0b11011111 do {
  Printf.printf "0x%02X00 " i;
  for j = 0b10000000 to 0b10111111 do {
    print_char (Char.chr i);
    print_char (Char.chr j);
  };
  print_endline "";
};
print_endline "";

for i = 0b11100000 to 0b11101111 do {
  for j = 0b10000000 to 0b10111111 do {
     Printf.printf "0x%02X%02X00 " i j;
    for k = 0b10000000 to 0b10111111 do {
      print_char (Char.chr i);
      print_char (Char.chr j);
      print_char (Char.chr k);
    };
    print_endline "";
  };
  print_endline "";
};
print_endline "";

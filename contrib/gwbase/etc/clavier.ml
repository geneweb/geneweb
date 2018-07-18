(* $Id: clavier.ml,v 4.1 2006-12-23 22:47:37 deraugla Exp $ *)

let main () =
  for i = 0 to 255 do
    if i >= 32 && i < 128 || i >= 161 then Printf.printf "%c " (Char.chr i)
    else Printf.printf ". ";
    if i mod 8 == 7 then Printf.printf "\n"
  done;
  flush stdout

let _ = Printexc.print main ()


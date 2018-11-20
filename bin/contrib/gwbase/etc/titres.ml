open Geneweb
open Def
open Gwdb

module Buff = Buff.Make (struct  end)

let lower_utf_8 s =
  let rec loop i len =
    if i >= String.length s then Buff.get len
    else
      let c =
        if Name.nbc s.[i] = 1 then Char.lowercase_ascii s.[i] else s.[i]
      in
      loop (i + 1) (Buff.store len c)
  in
  loop 0 0

let titres bname =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    List.iter
      (fun t ->
         Printf.printf "%s/%s\n" (lower_utf_8 (sou base t.t_ident))
           (lower_utf_8 (sou base t.t_place)))
      (get_titles p)
  done;
  flush stdout

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: titres base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  titres !bname

let _ = main ()

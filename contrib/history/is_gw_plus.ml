(* camlp5r *)

open Def;

type gen_record = 
  { date : string;
    wizard : string;
    gen_p : gen_person iper string;
    gen_f : list (gen_family iper string);
    gen_c : list (array iper) }
;

value test_history fname pos =
  match try Some (Secure.open_in_bin fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        try 
          do {
            seek_in ic pos;
            let v : gen_record = input_value ic in
            let _ = List.length v.gen_p.pevents in
            ()
          }
        with [ End_of_file -> () ];
        close_in ic
      }
  | None -> () ]
;

value main () = do {
  let fname = Sys.argv.(1) in
  let pos = try int_of_string Sys.argv.(2) with [Failure _ -> 0] in
  test_history fname pos;
};

main ();












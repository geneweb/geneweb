
open Def

type gen_record =
  { date : string;
    wizard : string;
    gen_p : (iper, string) gen_person;
    gen_f : (iper, string) gen_family list;
    gen_c : iper array list }

let test_history fname pos =
  match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
    Some ic ->
      begin try
        seek_in ic pos;
        let v : gen_record = input_value ic in
        let _ = List.length v.gen_p.pevents in ()
      with End_of_file -> ()
      end;
      close_in ic
  | None -> ()

let main () =
  let fname = Sys.argv.(1) in
  let pos = try int_of_string Sys.argv.(2) with Failure _ -> 0 in
  test_history fname pos

let _ = main ()












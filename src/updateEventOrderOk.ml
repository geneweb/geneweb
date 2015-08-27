open Config;
open Def;
open Futil;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;
open Printf;

value print_reorder_ok conf base person =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "Person events order changed")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.wprint "\n%s" (referenced_person_text conf base person);
    Wserver.wprint "\n";
    trailer conf;
  }
;

module IntSet = Set.Make (struct
                            type t = int;
                            value compare = compare;
                         end);

value verify_permutation expected_len xs =
  let len = List.length xs in
  (len = expected_len) &&
  (* all values in  range *)
  List.for_all (fun x -> (0<=x) && (x<len)) xs &&
  (* no dublicates *)
  fst (List.fold_left (fun (ans,set) x -> if IntSet.mem x set then (False,set)
                                          else (ans, IntSet.add x set)
                      ) (True, IntSet.empty) xs)
;

value apply_permutation perm xs =
  List.map (List.nth xs) perm
;

value print_mod conf base =
  let order =
    match p_getenv conf.env "v" with
    [ Some s -> let xs = Str.split (Str.regexp ",") s in
                Some (List.map int_of_string xs)
    | None -> None ]
  in
  let p =
    match p_getint conf.env "i" with
    [ Some ip -> Some (Adef.iper_of_int ip)
    | None -> None ]
  in

  (* TODO: more checks for parameters *)
  (* TODO: check for authorization *)
  match (p,order) with
  [ (Some iper, Some order) ->
    let person = poi base iper in
    let gen_p = gen_person_of_person person in
    let new_order = apply_permutation order gen_p.pevents in
    let () = patch_person base iper { (gen_p) with pevents = new_order } in
    let () = Util.commit_patches conf base in
    print_reorder_ok conf base person
  |  _ ->          Wserver.wprint "<h3 class=\"error\">"  ]
;

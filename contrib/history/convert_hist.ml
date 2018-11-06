open Geneweb
open Def

(**/**) (* Ancien format de stockage *)


type ('person, 'string) old_gen_person =
  { old_first_name : 'string;
    old_surname : 'string;
    old_occ : int;
    old_image : 'string;
    old_public_name : 'string;
    old_qualifiers : 'string list;
    old_aliases : 'string list;
    old_first_names_aliases : 'string list;
    old_surnames_aliases : 'string list;
    old_titles : 'string gen_title list;
    old_rparents : ('person, 'string) gen_relation list;
    old_related : iper list;
    old_occupation : 'string;
    old_sex : sex;
    old_access : access;
    old_birth : cdate;
    old_birth_place : 'string;
    old_birth_src : 'string;
    old_baptism : cdate;
    old_baptism_place : 'string;
    old_baptism_src : 'string;
    old_death : death;
    old_death_place : 'string;
    old_death_src : 'string;
    old_burial : burial;
    old_burial_place : 'string;
    old_burial_src : 'string;
    old_notes : 'string;
    old_psources : 'string;
    old_key_index : iper }

type ('person, 'string) old_gen_family =
  { old_marriage : cdate;
    old_marriage_place : 'string;
    old_marriage_src : 'string;
    old_witnesses : 'person array;
    old_relation : relation_kind;
    old_divorce : divorce;
    old_comment : 'string;
    old_origin_file : 'string;
    old_fsources : 'string;
    old_fam_index : ifam }


type old_gen_record =
  { old_date : string;
    old_wizard : string;
    old_gen_p : (iper, string) old_gen_person;
    old_gen_f : (iper, string) old_gen_family list;
    old_gen_c : iper array list }

type gen_record =
  { date : string;
    wizard : string;
    gen_p : (iper, string) gen_person;
    gen_f : (iper, string) gen_family list;
    gen_c : iper array list }


(* Fonction de conversion *)

let load_old_person_history fname =
  let history = ref [] in
  begin match
    (try Some (Secure.open_in_bin fname) with Sys_error _ -> None)
  with
    Some ic ->
      begin try
        while true do
          let v : old_gen_record = input_value ic in history := v :: !history
        done
      with End_of_file -> ()
      end;
      close_in ic
  | None -> ()
  end;
  (* On retourne la liste car les dernières  *)
  (* entrées se retrouvent en tête de liste. *)
  List.rev !history

let convert_file file tmp_file =
  let old_history = load_old_person_history file in
  let new_history =
    List.map
      (fun old_gr ->
         let old_gen_p = old_gr.old_gen_p in
         let gen_p =
           {first_name = old_gen_p.old_first_name;
            surname = old_gen_p.old_surname; occ = old_gen_p.old_occ;
            image = old_gen_p.old_image;
            first_names_aliases = old_gen_p.old_first_names_aliases;
            surnames_aliases = old_gen_p.old_surnames_aliases;
            public_name = old_gen_p.old_public_name;
            qualifiers = old_gen_p.old_qualifiers;
            titles = old_gen_p.old_titles; rparents = old_gen_p.old_rparents;
            related = old_gen_p.old_related; aliases = old_gen_p.old_aliases;
            occupation = old_gen_p.old_occupation; sex = old_gen_p.old_sex;
            access = old_gen_p.old_access; birth = old_gen_p.old_birth;
            birth_place = old_gen_p.old_birth_place; birth_note = "";
            birth_src = old_gen_p.old_birth_src;
            baptism = old_gen_p.old_baptism;
            baptism_place = old_gen_p.old_baptism_place; baptism_note = "";
            baptism_src = old_gen_p.old_baptism_src;
            death = old_gen_p.old_death;
            death_place = old_gen_p.old_death_place; death_note = "";
            death_src = old_gen_p.old_death_src;
            burial = old_gen_p.old_burial;
            burial_place = old_gen_p.old_burial_place; burial_note = "";
            burial_src = old_gen_p.old_burial_src; pevents = [];
            notes = old_gen_p.old_notes; psources = old_gen_p.old_psources;
            key_index = old_gen_p.old_key_index}
         in
         let gen_f =
           List.map
             (fun old_gen_f ->
                {marriage = old_gen_f.old_marriage;
                 marriage_place = old_gen_f.old_marriage_place;
                 marriage_note = "";
                 marriage_src = old_gen_f.old_marriage_src;
                 relation = old_gen_f.old_relation;
                 divorce = old_gen_f.old_divorce; fevents = [];
                 witnesses = old_gen_f.old_witnesses;
                 comment = old_gen_f.old_comment;
                 origin_file = old_gen_f.old_origin_file;
                 fsources = old_gen_f.old_fsources;
                 fam_index = old_gen_f.old_fam_index})
             old_gr.old_gen_f
         in
         {date = old_gr.old_date; wizard = old_gr.old_wizard; gen_p = gen_p;
          gen_f = gen_f; gen_c = old_gr.old_gen_c})
      old_history
  in
  let ext_flags =
    [Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 tmp_file) with
      Sys_error _ -> None
  with
    Some oc ->
      List.iter (fun gr -> output_value oc (gr : gen_record)) new_history;
      close_out oc
  | None -> ()

let convert history_dir =
  let files =
    Util.ls_r [ history_dir ]
    |> List.filter (fun x -> not @@ Sys.is_directory x)
  in
  let len = List.length files in
  ProgrBar.start ();
  let rec loop i = function
    | [] -> ()
    | file :: l ->
      let tmp_file = file ^ ".new" in
      convert_file file tmp_file;
      Sys.rename file (file ^ "~");
      Sys.rename tmp_file file;
      ProgrBar.run i len;
      loop (i + 1) l
  in
  loop 0 files ;
  ProgrBar.finish ()

(**/**) (* main *)

let history_dir = ref ""

let speclist = []
let anonfun n = history_dir := n
let usage = "Usage: convert_hist history_dir (the history_d folder)"


let main () =
  Arg.parse speclist anonfun usage;
  if !history_dir = "" then begin Arg.usage speclist usage; exit 2 end;
  convert !history_dir

let _ = main ()

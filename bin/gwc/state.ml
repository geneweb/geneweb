module Person_reference = struct
  type t = { pk_first_name : string; pk_surname : string; pk_occ : int }

  let normalize person_reference =
    {
      person_reference with
      pk_first_name = Name.lower person_reference.pk_first_name;
      pk_surname = Name.lower person_reference.pk_surname;
    }

  let compare person_reference person_reference' =
    compare (normalize person_reference) (normalize person_reference')

  let is_homonymous person_reference person_reference' =
    let person_reference = normalize person_reference in
    let person_reference' = normalize person_reference' in
    String.equal person_reference.pk_first_name person_reference'.pk_first_name
    && String.equal person_reference.pk_surname person_reference'.pk_surname
end

module Person_reference_set = Set.Make (Person_reference)
module Person_reference_map = Map.Make (Person_reference)

type person_references = {
  valid_references : Person_reference_set.t;
  fixed_occurrence_numbers : int option Person_reference_map.t;
}

let add_person_reference ~person_references
    (person_reference : Person_reference.t) =
  if Occurrence_number.is_valid person_reference.pk_occ then
    {
      person_references with
      valid_references =
        Person_reference_set.add
          (Person_reference.normalize person_reference)
          person_references.valid_references;
    }
  else
    {
      person_references with
      fixed_occurrence_numbers =
        Person_reference_map.add
          (Person_reference.normalize person_reference)
          None person_references.fixed_occurrence_numbers;
    }

let add_fixed_occurrence_number ~person_references ~person_reference
    occurrence_number =
  {
    person_references with
    fixed_occurrence_numbers =
      Person_reference_map.add
        (Person_reference.normalize person_reference)
        (Some occurrence_number) person_references.fixed_occurrence_numbers;
  }

let find_free_occurrence_number ~person_references person_reference =
  let homonyms =
    Person_reference_set.filter
      (Person_reference.is_homonymous person_reference)
      person_references
  in
  Occurrence_number.smallest_free @@ Ext_int.Set.of_list
  @@ List.map
       (fun (person_reference : Person_reference.t) -> person_reference.pk_occ)
       (Person_reference_set.elements homonyms)

let fix_invalid_person_reference ~person_references person_reference =
  let pk_occ =
    find_free_occurrence_number
      ~person_references:person_references.valid_references person_reference
  in
  let fixed_person_reference = { person_reference with pk_occ } in
  let person_references =
    add_person_reference ~person_references fixed_person_reference
  in
  ( fixed_person_reference,
    add_fixed_occurrence_number ~person_references ~person_reference pk_occ )

type t = {
  just_comp : bool;
  out_file : string;
  force : bool;
  separate : bool;
  (* TODO use type for bnotes *)
  bnotes : string;
  shift : int;
  no_fail : bool;
  no_picture : bool;
  no_public : bool;
  mutable create_all_keys : bool;
  mutable files : (string * bool * string * int) list;
  mutable line_cnt : int;
  mutable person_references : person_references;
  default_source : string;
  do_check : bool;
  do_consang : bool;
  pr_stats : bool;
  particules_file : string;
}

let default =
  {
    just_comp = false;
    out_file = Filename.concat Filename.current_dir_name "a";
    force = false;
    separate = false;
    bnotes = "merge";
    shift = 0;
    files = [];
    no_fail = false;
    no_picture = false;
    no_public = false;
    create_all_keys = false;
    line_cnt = 0;
    person_references =
      {
        valid_references = Person_reference_set.empty;
        fixed_occurrence_numbers = Person_reference_map.empty;
      };
    default_source = "";
    do_check = true;
    do_consang = false;
    pr_stats = false;
    particules_file = "";
  }

let add_person_reference state person_reference =
  let person_references =
    add_person_reference ~person_references:state.person_references
      person_reference
  in
  state.person_references <- person_references

let fix_invalid_person_reference state person_reference =
  let person_reference, person_references =
    fix_invalid_person_reference ~person_references:state.person_references
      person_reference
  in
  state.person_references <- person_references;
  person_reference

let get_fixed_person_reference state (person_reference : Person_reference.t) =
  match
    Person_reference_map.find_opt person_reference
      state.person_references.fixed_occurrence_numbers
  with
  | None -> person_reference
  | Some (Some pk_occ) -> { person_reference with pk_occ }
  | Some None -> fix_invalid_person_reference state person_reference

let all_person_references_are_valid state =
  Person_reference_map.for_all (Fun.const Option.is_some)
    state.person_references.fixed_occurrence_numbers

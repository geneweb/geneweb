module rec Api_link_tree_piqi:
  sig
    type protobuf_int32 = int32
    type calendar =
      [
        | `gregorian
        | `julian
        | `french
        | `hebrew
      ]
    type precision =
      [
        | `sure
        | `about
        | `maybe
        | `before
        | `after
        | `oryear
        | `yearint
      ]
    type sex =
      [
        | `male
        | `female
        | `unknown
      ]
    type death_type =
      [
        | `not_dead
        | `dead
        | `dead_young
        | `dead_dont_know_when
        | `dont_know_if_dead
        | `of_course_dead
      ]
    type title_type =
      [
        | `title_main
        | `title_name
        | `title_none
      ]
    type marriage_type =
      [
        | `married
        | `not_married
        | `engaged
        | `no_sexes_check_not_married
        | `no_mention
        | `no_sexes_check_married
      ]
    type divorce_type =
      [
        | `not_divorced
        | `divorced
        | `separated
      ]
    type dmy = Dmy.t
    type date = Date.t
    type title = Title.t
    type connection = Connection.t
    type family_link = Family_link.t
    type person_link = Person_link.t
    type person = Person.t
    type family = Family.t
    type link_tree = Link_tree.t
    type link_tree_params = Link_tree_params.t
  end = Api_link_tree_piqi
and Dmy:
  sig
    type t = {
      mutable day: Api_link_tree_piqi.protobuf_int32 option;
      mutable month: Api_link_tree_piqi.protobuf_int32 option;
      mutable year: int32 option;
      mutable delta: Api_link_tree_piqi.protobuf_int32 option;
    }
  end = Dmy
and Date:
  sig
    type t = {
      mutable cal: Api_link_tree_piqi.calendar option;
      mutable prec: Api_link_tree_piqi.precision option;
      mutable dmy: Api_link_tree_piqi.dmy option;
      mutable dmy2: Api_link_tree_piqi.dmy option;
      mutable text: string option;
    }
  end = Date
and Title:
  sig
    type t = {
      mutable title_type: Api_link_tree_piqi.title_type;
      mutable name: string;
      mutable title: string;
      mutable fief: string;
      mutable date_begin: string;
      mutable date_end: string;
      mutable nth: Api_link_tree_piqi.protobuf_int32;
    }
  end = Title
and Connection:
  sig
    type t = {
      mutable from_baseprefix: string;
      mutable from_ref: string;
      mutable to_baseprefix: string;
      mutable to_ref: string;
    }
  end = Connection
and Family_link:
  sig
    type t = {
      mutable baseprefix: string;
      mutable ifam: Api_link_tree_piqi.protobuf_int32;
    }
  end = Family_link
and Person_link:
  sig
    type t = {
      mutable baseprefix: string;
      mutable ip: Api_link_tree_piqi.protobuf_int32;
    }
  end = Person_link
and Person:
  sig
    type t = {
      mutable baseprefix: string;
      mutable ip: Api_link_tree_piqi.protobuf_int32;
      mutable n: string;
      mutable p: string;
      mutable oc: Api_link_tree_piqi.protobuf_int32;
      mutable lastname: string;
      mutable firstname: string;
      mutable image: string option;
      mutable occupation: string option;
      mutable public_name: string option;
      mutable qualifiers: string list;
      mutable titles: string list;
      mutable aliases: string list;
      mutable sex: Api_link_tree_piqi.sex;
      mutable birth_date: Api_link_tree_piqi.date option;
      mutable birth_place: string option;
      mutable baptism_date: Api_link_tree_piqi.date option;
      mutable baptism_place: string option;
      mutable death_type: Api_link_tree_piqi.death_type;
      mutable death_date: Api_link_tree_piqi.date option;
      mutable death_place: string option;
      mutable burial_date: Api_link_tree_piqi.date option;
      mutable burial_place: string option;
      mutable families: Api_link_tree_piqi.family_link list;
    }
  end = Person
and Family:
  sig
    type t = {
      mutable baseprefix: string;
      mutable ifam: Api_link_tree_piqi.protobuf_int32;
      mutable ifath: Api_link_tree_piqi.protobuf_int32;
      mutable imoth: Api_link_tree_piqi.protobuf_int32;
      mutable marriage_type: Api_link_tree_piqi.marriage_type;
      mutable marriage_date: Api_link_tree_piqi.date option;
      mutable marriage_place: string option;
      mutable divorce_type: Api_link_tree_piqi.divorce_type;
      mutable divorce_date: Api_link_tree_piqi.date option;
      mutable children: Api_link_tree_piqi.person_link list;
    }
  end = Family
and Link_tree:
  sig
    type t = {
      mutable families: Api_link_tree_piqi.family list;
      mutable persons: Api_link_tree_piqi.person list;
      mutable connections: Api_link_tree_piqi.connection list;
    }
  end = Link_tree
and Link_tree_params:
  sig
    type t = {
      mutable basename: string;
      mutable ip: Api_link_tree_piqi.protobuf_int32 option;
      mutable ref_person: string option;
      mutable ref_person2: string option;
      mutable nb_asc: Api_link_tree_piqi.protobuf_int32;
      mutable from_gen_desc: Api_link_tree_piqi.protobuf_int32;
      mutable nb_desc: Api_link_tree_piqi.protobuf_int32;
    }
  end = Link_tree_params


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_dmy x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_optional_field 3 parse_int32 x in
  let _delta, x = Piqirun.parse_optional_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Dmy.day = _day;
    Dmy.month = _month;
    Dmy.year = _year;
    Dmy.delta = _delta;
  }

and parse_date x =
  let x = Piqirun.parse_record x in
  let _cal, x = Piqirun.parse_optional_field 1 parse_calendar x in
  let _prec, x = Piqirun.parse_optional_field 2 parse_precision x in
  let _dmy, x = Piqirun.parse_optional_field 3 parse_dmy x in
  let _dmy2, x = Piqirun.parse_optional_field 4 parse_dmy x in
  let _text, x = Piqirun.parse_optional_field 5 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Date.cal = _cal;
    Date.prec = _prec;
    Date.dmy = _dmy;
    Date.dmy2 = _dmy2;
    Date.text = _text;
  }

and parse_title x =
  let x = Piqirun.parse_record x in
  let _title_type, x = Piqirun.parse_required_field 1 parse_title_type x in
  let _name, x = Piqirun.parse_required_field 2 parse_string x in
  let _title, x = Piqirun.parse_required_field 3 parse_string x in
  let _fief, x = Piqirun.parse_required_field 4 parse_string x in
  let _date_begin, x = Piqirun.parse_required_field 5 parse_string x in
  let _date_end, x = Piqirun.parse_required_field 6 parse_string x in
  let _nth, x = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Title.title_type = _title_type;
    Title.name = _name;
    Title.title = _title;
    Title.fief = _fief;
    Title.date_begin = _date_begin;
    Title.date_end = _date_end;
    Title.nth = _nth;
  }

and parse_connection x =
  let x = Piqirun.parse_record x in
  let _from_baseprefix, x = Piqirun.parse_required_field 1 parse_string x in
  let _from_ref, x = Piqirun.parse_required_field 2 parse_string x in
  let _to_baseprefix, x = Piqirun.parse_required_field 3 parse_string x in
  let _to_ref, x = Piqirun.parse_required_field 4 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Connection.from_baseprefix = _from_baseprefix;
    Connection.from_ref = _from_ref;
    Connection.to_baseprefix = _to_baseprefix;
    Connection.to_ref = _to_ref;
  }

and parse_family_link x =
  let x = Piqirun.parse_record x in
  let _baseprefix, x = Piqirun.parse_required_field 1 parse_string x in
  let _ifam, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Family_link.baseprefix = _baseprefix;
    Family_link.ifam = _ifam;
  }

and parse_person_link x =
  let x = Piqirun.parse_record x in
  let _baseprefix, x = Piqirun.parse_required_field 1 parse_string x in
  let _ip, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Person_link.baseprefix = _baseprefix;
    Person_link.ip = _ip;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _baseprefix, x = Piqirun.parse_required_field 1 parse_string x in
  let _ip, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _n, x = Piqirun.parse_required_field 3 parse_string x in
  let _p, x = Piqirun.parse_required_field 4 parse_string x in
  let _oc, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _lastname, x = Piqirun.parse_required_field 6 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 7 parse_string x in
  let _image, x = Piqirun.parse_optional_field 8 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 9 parse_string x in
  let _public_name, x = Piqirun.parse_optional_field 10 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 11 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 12 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 13 parse_string x in
  let _sex, x = Piqirun.parse_required_field 14 parse_sex x in
  let _birth_date, x = Piqirun.parse_optional_field 15 parse_date x in
  let _birth_place, x = Piqirun.parse_optional_field 16 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 17 parse_date x in
  let _baptism_place, x = Piqirun.parse_optional_field 18 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 19 parse_death_type x in
  let _death_date, x = Piqirun.parse_optional_field 20 parse_date x in
  let _death_place, x = Piqirun.parse_optional_field 21 parse_string x in
  let _burial_date, x = Piqirun.parse_optional_field 22 parse_date x in
  let _burial_place, x = Piqirun.parse_optional_field 23 parse_string x in
  let _families, x = Piqirun.parse_repeated_field 24 parse_family_link x in
  Piqirun.check_unparsed_fields x;
  {
    Person.baseprefix = _baseprefix;
    Person.ip = _ip;
    Person.n = _n;
    Person.p = _p;
    Person.oc = _oc;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.image = _image;
    Person.occupation = _occupation;
    Person.public_name = _public_name;
    Person.qualifiers = _qualifiers;
    Person.titles = _titles;
    Person.aliases = _aliases;
    Person.sex = _sex;
    Person.birth_date = _birth_date;
    Person.birth_place = _birth_place;
    Person.baptism_date = _baptism_date;
    Person.baptism_place = _baptism_place;
    Person.death_type = _death_type;
    Person.death_date = _death_date;
    Person.death_place = _death_place;
    Person.burial_date = _burial_date;
    Person.burial_place = _burial_place;
    Person.families = _families;
  }

and parse_family x =
  let x = Piqirun.parse_record x in
  let _baseprefix, x = Piqirun.parse_required_field 1 parse_string x in
  let _ifam, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _ifath, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _imoth, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  let _marriage_type, x = Piqirun.parse_required_field 5 parse_marriage_type x in
  let _marriage_date, x = Piqirun.parse_optional_field 6 parse_date x in
  let _marriage_place, x = Piqirun.parse_optional_field 7 parse_string x in
  let _divorce_type, x = Piqirun.parse_required_field 8 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 9 parse_date x in
  let _children, x = Piqirun.parse_repeated_field 10 parse_person_link x in
  Piqirun.check_unparsed_fields x;
  {
    Family.baseprefix = _baseprefix;
    Family.ifam = _ifam;
    Family.ifath = _ifath;
    Family.imoth = _imoth;
    Family.marriage_type = _marriage_type;
    Family.marriage_date = _marriage_date;
    Family.marriage_place = _marriage_place;
    Family.divorce_type = _divorce_type;
    Family.divorce_date = _divorce_date;
    Family.children = _children;
  }

and parse_link_tree x =
  let x = Piqirun.parse_record x in
  let _families, x = Piqirun.parse_repeated_field 1 parse_family x in
  let _persons, x = Piqirun.parse_repeated_field 2 parse_person x in
  let _connections, x = Piqirun.parse_repeated_field 3 parse_connection x in
  Piqirun.check_unparsed_fields x;
  {
    Link_tree.families = _families;
    Link_tree.persons = _persons;
    Link_tree.connections = _connections;
  }

and parse_link_tree_params x =
  let x = Piqirun.parse_record x in
  let _basename, x = Piqirun.parse_required_field 1 parse_string x in
  let _ip, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _ref_person, x = Piqirun.parse_optional_field 3 parse_string x in
  let _ref_person2, x = Piqirun.parse_optional_field 4 parse_string x in
  let _nb_asc, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _from_gen_desc, x = Piqirun.parse_required_field 6 parse_protobuf_int32 x in
  let _nb_desc, x = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Link_tree_params.basename = _basename;
    Link_tree_params.ip = _ip;
    Link_tree_params.ref_person = _ref_person;
    Link_tree_params.ref_person2 = _ref_person2;
    Link_tree_params.nb_asc = _nb_asc;
    Link_tree_params.from_gen_desc = _from_gen_desc;
    Link_tree_params.nb_desc = _nb_desc;
  }

and parse_calendar x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x
and packed_parse_calendar x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x

and parse_precision x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x
and packed_parse_precision x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x

and parse_sex x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x
and packed_parse_sex x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x

and parse_death_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x
and packed_parse_death_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x

and parse_title_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x
and packed_parse_title_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x

and parse_marriage_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x
and packed_parse_marriage_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x

and parse_divorce_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x
and packed_parse_divorce_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__dmy code x =
  let _day = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_optional_field 3 gen__int32 x.Dmy.year in
  let _delta = Piqirun.gen_optional_field 4 gen__protobuf_int32 x.Dmy.delta in
  Piqirun.gen_record code (_day :: _month :: _year :: _delta :: [])

and gen__date code x =
  let _cal = Piqirun.gen_optional_field 1 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 2 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 3 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 5 gen__string x.Date.text in
  Piqirun.gen_record code (_cal :: _prec :: _dmy :: _dmy2 :: _text :: [])

and gen__title code x =
  let _title_type = Piqirun.gen_required_field 1 gen__title_type x.Title.title_type in
  let _name = Piqirun.gen_required_field 2 gen__string x.Title.name in
  let _title = Piqirun.gen_required_field 3 gen__string x.Title.title in
  let _fief = Piqirun.gen_required_field 4 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_required_field 5 gen__string x.Title.date_begin in
  let _date_end = Piqirun.gen_required_field 6 gen__string x.Title.date_end in
  let _nth = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_title_type :: _name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__connection code x =
  let _from_baseprefix = Piqirun.gen_required_field 1 gen__string x.Connection.from_baseprefix in
  let _from_ref = Piqirun.gen_required_field 2 gen__string x.Connection.from_ref in
  let _to_baseprefix = Piqirun.gen_required_field 3 gen__string x.Connection.to_baseprefix in
  let _to_ref = Piqirun.gen_required_field 4 gen__string x.Connection.to_ref in
  Piqirun.gen_record code (_from_baseprefix :: _from_ref :: _to_baseprefix :: _to_ref :: [])

and gen__family_link code x =
  let _baseprefix = Piqirun.gen_required_field 1 gen__string x.Family_link.baseprefix in
  let _ifam = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Family_link.ifam in
  Piqirun.gen_record code (_baseprefix :: _ifam :: [])

and gen__person_link code x =
  let _baseprefix = Piqirun.gen_required_field 1 gen__string x.Person_link.baseprefix in
  let _ip = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Person_link.ip in
  Piqirun.gen_record code (_baseprefix :: _ip :: [])

and gen__person code x =
  let _baseprefix = Piqirun.gen_required_field 1 gen__string x.Person.baseprefix in
  let _ip = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Person.ip in
  let _n = Piqirun.gen_required_field 3 gen__string x.Person.n in
  let _p = Piqirun.gen_required_field 4 gen__string x.Person.p in
  let _oc = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Person.oc in
  let _lastname = Piqirun.gen_required_field 6 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 7 gen__string x.Person.firstname in
  let _image = Piqirun.gen_optional_field 8 gen__string x.Person.image in
  let _occupation = Piqirun.gen_optional_field 9 gen__string x.Person.occupation in
  let _public_name = Piqirun.gen_optional_field 10 gen__string x.Person.public_name in
  let _qualifiers = Piqirun.gen_repeated_field 11 gen__string x.Person.qualifiers in
  let _titles = Piqirun.gen_repeated_field 12 gen__string x.Person.titles in
  let _aliases = Piqirun.gen_repeated_field 13 gen__string x.Person.aliases in
  let _sex = Piqirun.gen_required_field 14 gen__sex x.Person.sex in
  let _birth_date = Piqirun.gen_optional_field 15 gen__date x.Person.birth_date in
  let _birth_place = Piqirun.gen_optional_field 16 gen__string x.Person.birth_place in
  let _baptism_date = Piqirun.gen_optional_field 17 gen__date x.Person.baptism_date in
  let _baptism_place = Piqirun.gen_optional_field 18 gen__string x.Person.baptism_place in
  let _death_type = Piqirun.gen_required_field 19 gen__death_type x.Person.death_type in
  let _death_date = Piqirun.gen_optional_field 20 gen__date x.Person.death_date in
  let _death_place = Piqirun.gen_optional_field 21 gen__string x.Person.death_place in
  let _burial_date = Piqirun.gen_optional_field 22 gen__date x.Person.burial_date in
  let _burial_place = Piqirun.gen_optional_field 23 gen__string x.Person.burial_place in
  let _families = Piqirun.gen_repeated_field 24 gen__family_link x.Person.families in
  Piqirun.gen_record code (_baseprefix :: _ip :: _n :: _p :: _oc :: _lastname :: _firstname :: _image :: _occupation :: _public_name :: _qualifiers :: _titles :: _aliases :: _sex :: _birth_date :: _birth_place :: _baptism_date :: _baptism_place :: _death_type :: _death_date :: _death_place :: _burial_date :: _burial_place :: _families :: [])

and gen__family code x =
  let _baseprefix = Piqirun.gen_required_field 1 gen__string x.Family.baseprefix in
  let _ifam = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Family.ifam in
  let _ifath = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Family.ifath in
  let _imoth = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Family.imoth in
  let _marriage_type = Piqirun.gen_required_field 5 gen__marriage_type x.Family.marriage_type in
  let _marriage_date = Piqirun.gen_optional_field 6 gen__date x.Family.marriage_date in
  let _marriage_place = Piqirun.gen_optional_field 7 gen__string x.Family.marriage_place in
  let _divorce_type = Piqirun.gen_required_field 8 gen__divorce_type x.Family.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 9 gen__date x.Family.divorce_date in
  let _children = Piqirun.gen_repeated_field 10 gen__person_link x.Family.children in
  Piqirun.gen_record code (_baseprefix :: _ifam :: _ifath :: _imoth :: _marriage_type :: _marriage_date :: _marriage_place :: _divorce_type :: _divorce_date :: _children :: [])

and gen__link_tree code x =
  let _families = Piqirun.gen_repeated_field 1 gen__family x.Link_tree.families in
  let _persons = Piqirun.gen_repeated_field 2 gen__person x.Link_tree.persons in
  let _connections = Piqirun.gen_repeated_field 3 gen__connection x.Link_tree.connections in
  Piqirun.gen_record code (_families :: _persons :: _connections :: [])

and gen__link_tree_params code x =
  let _basename = Piqirun.gen_required_field 1 gen__string x.Link_tree_params.basename in
  let _ip = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Link_tree_params.ip in
  let _ref_person = Piqirun.gen_optional_field 3 gen__string x.Link_tree_params.ref_person in
  let _ref_person2 = Piqirun.gen_optional_field 4 gen__string x.Link_tree_params.ref_person2 in
  let _nb_asc = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Link_tree_params.nb_asc in
  let _from_gen_desc = Piqirun.gen_required_field 6 gen__protobuf_int32 x.Link_tree_params.from_gen_desc in
  let _nb_desc = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Link_tree_params.nb_desc in
  Piqirun.gen_record code (_basename :: _ip :: _ref_person :: _ref_person2 :: _nb_asc :: _from_gen_desc :: _nb_desc :: [])

and gen__calendar code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )
and packed_gen__calendar x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )

and gen__precision code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )
and packed_gen__precision x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )

and gen__sex code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )

and gen__death_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )

and gen__title_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )
and packed_gen__title_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )

and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )

and gen__divorce_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )
and packed_gen__divorce_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )


let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_dmy x = gen__dmy (-1) x
let gen_date x = gen__date (-1) x
let gen_title x = gen__title (-1) x
let gen_connection x = gen__connection (-1) x
let gen_family_link x = gen__family_link (-1) x
let gen_person_link x = gen__person_link (-1) x
let gen_person x = gen__person (-1) x
let gen_family x = gen__family (-1) x
let gen_link_tree x = gen__link_tree (-1) x
let gen_link_tree_params x = gen__link_tree_params (-1) x
let gen_calendar x = gen__calendar (-1) x
let gen_precision x = gen__precision (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_title_type x = gen__title_type (-1) x
let gen_marriage_type x = gen__marriage_type (-1) x
let gen_divorce_type x = gen__divorce_type (-1) x


let rec default_int32 () = 0l
and default_protobuf_int32 () = default_int32 ()
and default_string () = ""
and default_dmy () =
  {
    Dmy.day = None;
    Dmy.month = None;
    Dmy.year = None;
    Dmy.delta = None;
  }
and default_date () =
  {
    Date.cal = None;
    Date.prec = None;
    Date.dmy = None;
    Date.dmy2 = None;
    Date.text = None;
  }
and default_title () =
  {
    Title.title_type = default_title_type ();
    Title.name = default_string ();
    Title.title = default_string ();
    Title.fief = default_string ();
    Title.date_begin = default_string ();
    Title.date_end = default_string ();
    Title.nth = default_protobuf_int32 ();
  }
and default_connection () =
  {
    Connection.from_baseprefix = default_string ();
    Connection.from_ref = default_string ();
    Connection.to_baseprefix = default_string ();
    Connection.to_ref = default_string ();
  }
and default_family_link () =
  {
    Family_link.baseprefix = default_string ();
    Family_link.ifam = default_protobuf_int32 ();
  }
and default_person_link () =
  {
    Person_link.baseprefix = default_string ();
    Person_link.ip = default_protobuf_int32 ();
  }
and default_person () =
  {
    Person.baseprefix = default_string ();
    Person.ip = default_protobuf_int32 ();
    Person.n = default_string ();
    Person.p = default_string ();
    Person.oc = default_protobuf_int32 ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.image = None;
    Person.occupation = None;
    Person.public_name = None;
    Person.qualifiers = [];
    Person.titles = [];
    Person.aliases = [];
    Person.sex = default_sex ();
    Person.birth_date = None;
    Person.birth_place = None;
    Person.baptism_date = None;
    Person.baptism_place = None;
    Person.death_type = default_death_type ();
    Person.death_date = None;
    Person.death_place = None;
    Person.burial_date = None;
    Person.burial_place = None;
    Person.families = [];
  }
and default_family () =
  {
    Family.baseprefix = default_string ();
    Family.ifam = default_protobuf_int32 ();
    Family.ifath = default_protobuf_int32 ();
    Family.imoth = default_protobuf_int32 ();
    Family.marriage_type = default_marriage_type ();
    Family.marriage_date = None;
    Family.marriage_place = None;
    Family.divorce_type = default_divorce_type ();
    Family.divorce_date = None;
    Family.children = [];
  }
and default_link_tree () =
  {
    Link_tree.families = [];
    Link_tree.persons = [];
    Link_tree.connections = [];
  }
and default_link_tree_params () =
  {
    Link_tree_params.basename = default_string ();
    Link_tree_params.ip = None;
    Link_tree_params.ref_person = None;
    Link_tree_params.ref_person2 = None;
    Link_tree_params.nb_asc = default_protobuf_int32 ();
    Link_tree_params.from_gen_desc = default_protobuf_int32 ();
    Link_tree_params.nb_desc = default_protobuf_int32 ();
  }
and default_calendar () = `gregorian
and default_precision () = `sure
and default_sex () = `male
and default_death_type () = `not_dead
and default_title_type () = `title_main
and default_marriage_type () = `married
and default_divorce_type () = `not_divorced


let piqi = "\226\202\2304\rapi_link_tree\226\231\249\238\001\024api_link_tree.proto.piqi\162\244\146\155\011\028geneweb.api.link_tree.object\218\244\134\182\012\208\001\138\233\142\251\014\201\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$$\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004year\210\171\158\194\006\005int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005delta\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\003dmy\218\244\134\182\012\224\001\138\233\142\251\014\217\001\210\203\242$&\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003cal\210\171\158\194\006\bcalendar\210\203\242$(\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004prec\210\171\158\194\006\tprecision\210\203\242$!\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003dmy\210\171\158\194\006\003dmy\210\203\242$\"\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004dmy2\210\171\158\194\006\003dmy\210\203\242$%\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\218\164\238\191\004\004date\218\244\134\182\012\212\002\138\233\142\251\014\205\002\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntitle-type\210\171\158\194\006\ntitle-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005title\210\171\158\194\006\006string\210\203\242$%\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004fief\210\171\158\194\006\006string\210\203\242$+\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\006string\210\203\242$)\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bdate-end\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003nth\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005title\218\244\134\182\012\217\001\138\233\142\251\014\210\001\210\203\242$0\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\015from-baseprefix\210\171\158\194\006\006string\210\203\242$)\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bfrom-ref\210\171\158\194\006\006string\210\203\242$.\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rto-baseprefix\210\171\158\194\006\006string\210\203\242$'\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006to-ref\210\171\158\194\006\006string\218\164\238\191\004\nconnection\218\244\134\182\012y\138\233\142\251\014s\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$-\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004ifam\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\011family-link\218\244\134\182\012w\138\233\142\251\014q\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$+\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002ip\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\011person-link\218\244\134\182\012\242\b\138\233\142\251\014\235\b\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$+\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002ip\210\171\158\194\006\014protobuf-int32\210\203\242$\"\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$+\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002oc\210\171\158\194\006\014protobuf-int32\210\203\242$)\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$&\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$,\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$+\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$'\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\006string\210\203\242$(\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$!\232\146\150q\028\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\004date\210\203\242$,\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\004date\210\203\242$.\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$/\232\146\150q&\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$)\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\004date\210\203\242$,\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\004date\210\203\242$-\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$.\232\146\150q0\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\011family-link\218\164\238\191\004\006person\218\244\134\182\012\149\004\138\233\142\251\014\142\004\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\nbaseprefix\210\171\158\194\006\006string\210\203\242$-\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004ifam\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005ifath\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005imoth\210\171\158\194\006\014protobuf-int32\210\203\242$5\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\004date\210\203\242$/\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$3\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$+\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\004date\210\203\242$.\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\011person-link\218\164\238\191\004\006family\218\244\134\182\012\166\001\138\233\142\251\014\159\001\210\203\242$)\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\006family\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007persons\210\171\158\194\006\006person\210\203\242$0\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\011connections\210\171\158\194\006\nconnection\218\164\238\191\004\tlink-tree\218\244\134\182\012\128\003\138\233\142\251\014\249\002\210\203\242$)\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\bbasename\210\171\158\194\006\006string\210\203\242$+\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\002ip\210\171\158\194\006\014protobuf-int32\210\203\242$+\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nref-person\210\171\158\194\006\006string\210\203\242$,\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011ref-person2\210\171\158\194\006\006string\210\203\242$/\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006nb-asc\210\171\158\194\006\014protobuf-int32\210\203\242$6\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rfrom-gen-desc\210\171\158\194\006\014protobuf-int32\210\203\242$0\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007nb-desc\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\016link-tree-params\218\244\134\182\012s\138\176\205\197\001m\218\164\238\191\004\bcalendar\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tgregorian\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006julian\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006french\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006hebrew\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\tprecision\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004sure\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005about\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005maybe\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006before\170\183\218\222\005\016\232\146\150q\b\218\164\238\191\004\005after\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006oryear\170\183\218\222\005\018\232\146\150q\012\218\164\238\191\004\007yearint\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012g\138\176\205\197\001a\218\164\238\191\004\ntitle-type\170\183\218\222\005\021\232\146\150q\000\218\164\238\191\004\ntitle-main\170\183\218\222\005\021\232\146\150q\002\218\164\238\191\004\ntitle-name\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ntitle-none\218\244\134\182\012\211\001\138\176\205\197\001\204\001\218\164\238\191\004\rmarriage-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007married\170\183\218\222\005\022\232\146\150q\002\218\164\238\191\004\011not-married\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007engaged\170\183\218\222\005%\232\146\150q\006\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005\021\232\146\150q\b\218\164\238\191\004\nno-mention\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022no-sexes-check-married\218\244\134\182\012h\138\176\205\197\001b\218\164\238\191\004\012divorce-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bdivorced\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tseparated"
include Api_link_tree_piqi

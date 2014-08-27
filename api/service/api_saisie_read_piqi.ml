module rec Api_saisie_read_piqi :
             sig
               type protobuf_int32 = int32
               
               type protobuf_int64 = int64
               
               type sosa = [ | `sosa_ref | `sosa | `no_sosa ]
               
               type calendar = [ | `gregorian | `julian | `french | `hebrew ]
               
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
               
               type sex = [ | `male | `female | `unknown ]
               
               type death_type =
                 [
                   | `not_dead
                   | `dead
                   | `dead_young
                   | `dead_dont_know_when
                   | `dont_know_if_dead
                   | `of_course_dead
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
                 [ | `not_divorced | `divorced | `separated
                 ]
               
               type relation_type =
                 [
                   | `rparent_adoption
                   | `rparent_recognition
                   | `rparent_candidate_parent
                   | `rparent_god_parent
                   | `rparent_foster_parent
                   | `rchild_adoption
                   | `rchild_recognition
                   | `rchild_candidate_parent
                   | `rchild_god_parent
                   | `rchild_foster_parent
                 ]
               
               type witness_type = [ | `witness | `witness_godparent ]
               
               type dmy = Dmy.t
               
               type date = Date.t
               
               type witness_event = Witness_event.t
               
               type event = Event.t
               
               type person_tree = Person_tree.t
               
               type simple_person = Simple_person.t
               
               type relation_person = Relation_person.t
               
               type event_witness = Event_witness.t
               
               type person = Person.t
               
               type family = Family.t
               
               type index_person = Index_person.t
               
               type node = Node.t
               
               type edge = Edge.t
               
               type graph_tree = Graph_tree.t
               
               type graph_tree_params = Graph_tree_params.t
               
             end = Api_saisie_read_piqi
and
  Dmy :
    sig
      type t =
        { mutable day : Api_saisie_read_piqi.protobuf_int32;
          mutable month : Api_saisie_read_piqi.protobuf_int32;
          mutable year : Api_saisie_read_piqi.protobuf_int32;
          mutable delta : Api_saisie_read_piqi.protobuf_int32
        }
      
    end = Dmy
and
  Date :
    sig
      type t =
        { mutable cal : Api_saisie_read_piqi.calendar option;
          mutable prec : Api_saisie_read_piqi.precision option;
          mutable dmy : Api_saisie_read_piqi.dmy option;
          mutable dmy2 : Api_saisie_read_piqi.dmy option;
          mutable text : string option
        }
      
    end = Date
and
  Witness_event :
    sig
      type t =
        { mutable witness_type : Api_saisie_read_piqi.witness_type;
          mutable witness : Api_saisie_read_piqi.simple_person
        }
      
    end = Witness_event
and
  Event :
    sig
      type t =
        { mutable name : string; mutable date : string option;
          mutable date_conv : string option;
          mutable date_cal : Api_saisie_read_piqi.calendar option;
          mutable place : string option; mutable reason : string option;
          mutable note : string option; mutable src : string option;
          mutable spouse : Api_saisie_read_piqi.simple_person option;
          mutable witnesses : Api_saisie_read_piqi.witness_event list
        }
      
    end = Event
and
  Person_tree :
    sig
      type t =
        { mutable index : Api_saisie_read_piqi.protobuf_int32;
          mutable sex : Api_saisie_read_piqi.sex; mutable lastname : string;
          mutable firstname : string; mutable n : string; mutable p : string;
          mutable occ : Api_saisie_read_piqi.protobuf_int32;
          mutable dates : string option; mutable image : string option;
          mutable sosa : Api_saisie_read_piqi.sosa;
          mutable has_more_infos : bool
        }
      
    end = Person_tree
and
  Simple_person :
    sig
      type t =
        { mutable index : Api_saisie_read_piqi.protobuf_int32;
          mutable sex : Api_saisie_read_piqi.sex; mutable lastname : string;
          mutable firstname : string; mutable n : string; mutable p : string;
          mutable occ : Api_saisie_read_piqi.protobuf_int32;
          mutable birth_short_date : string option;
          mutable birth_place : string option;
          mutable death_short_date : string option;
          mutable death_place : string option; mutable image : string option;
          mutable sosa : Api_saisie_read_piqi.sosa
        }
      
    end = Simple_person
and
  Relation_person :
    sig
      type t =
        { mutable r_type : Api_saisie_read_piqi.relation_type;
          mutable person : Api_saisie_read_piqi.simple_person
        }
      
    end = Relation_person
and
  Event_witness :
    sig
      type t =
        { mutable event_witness_type : string;
          mutable husband : Api_saisie_read_piqi.simple_person;
          mutable wife : Api_saisie_read_piqi.simple_person option
        }
      
    end = Event_witness
and
  Person :
    sig
      type t =
        { mutable index : Api_saisie_read_piqi.protobuf_int32;
          mutable sex : Api_saisie_read_piqi.sex; mutable lastname : string;
          mutable firstname : string; mutable n : string; mutable p : string;
          mutable occ : Api_saisie_read_piqi.protobuf_int32;
          mutable public_name : string option; mutable aliases : string list;
          mutable qualifiers : string list;
          mutable firstname_aliases : string list;
          mutable surname_aliases : string list;
          mutable image : string option; mutable birth_date : string option;
          mutable birth_date_conv : string option;
          mutable birth_date_cal : Api_saisie_read_piqi.calendar option;
          mutable birth_place : string option;
          mutable birth_src : string option;
          mutable baptism_date : string option;
          mutable baptism_date_conv : string option;
          mutable baptism_date_cal : Api_saisie_read_piqi.calendar option;
          mutable baptism_place : string option;
          mutable baptism_src : string option;
          mutable death_date : string option;
          mutable death_date_conv : string option;
          mutable death_date_cal : Api_saisie_read_piqi.calendar option;
          mutable death_place : string option;
          mutable death_src : string option;
          mutable death_type : Api_saisie_read_piqi.death_type;
          mutable burial_date : string option;
          mutable burial_date_conv : string option;
          mutable burial_date_cal : Api_saisie_read_piqi.calendar option;
          mutable burial_place : string option;
          mutable burial_src : string option;
          mutable occupation : string option; mutable notes : string option;
          mutable psources : string option; mutable has_sources : bool;
          mutable titles : string list;
          mutable related : Api_saisie_read_piqi.relation_person list;
          mutable rparents : Api_saisie_read_piqi.relation_person list;
          mutable father : Api_saisie_read_piqi.simple_person option;
          mutable mother : Api_saisie_read_piqi.simple_person option;
          mutable families : Api_saisie_read_piqi.family list;
          mutable sosa : Api_saisie_read_piqi.sosa;
          mutable events : Api_saisie_read_piqi.event list;
          mutable events_witnesses : Api_saisie_read_piqi.event_witness list
        }
      
    end = Person
and
  Family :
    sig
      type t =
        { mutable index : Api_saisie_read_piqi.protobuf_int32;
          mutable spouse : Api_saisie_read_piqi.simple_person;
          mutable marriage_date : string option;
          mutable marriage_date_conv : string option;
          mutable marriage_date_cal : Api_saisie_read_piqi.calendar option;
          mutable marriage_place : string option;
          mutable marriage_src : string option;
          mutable marriage_type : Api_saisie_read_piqi.marriage_type;
          mutable divorce_type : Api_saisie_read_piqi.divorce_type;
          mutable divorce_date : string option;
          mutable divorce_date_conv : string option;
          mutable divorce_date_cal : Api_saisie_read_piqi.calendar option;
          mutable witnesses : Api_saisie_read_piqi.simple_person list;
          mutable notes : string option; mutable fsources : string option;
          mutable children : Api_saisie_read_piqi.simple_person list
        }
      
    end = Family
and
  Index_person :
    sig type t = { mutable index : Api_saisie_read_piqi.protobuf_int32 }
         end =
    Index_person
and
  Node :
    sig
      type t =
        { mutable id : Api_saisie_read_piqi.protobuf_int64;
          mutable person : Api_saisie_read_piqi.person_tree;
          mutable ifam : Api_saisie_read_piqi.protobuf_int64 option
        }
      
    end = Node
and
  Edge :
    sig
      type t =
        { mutable from_node : Api_saisie_read_piqi.protobuf_int64;
          mutable to_node : Api_saisie_read_piqi.protobuf_int64
        }
      
    end = Edge
and
  Graph_tree :
    sig
      type t =
        { mutable nodes_asc : Api_saisie_read_piqi.node list;
          mutable edges_asc : Api_saisie_read_piqi.edge list;
          mutable nodes_desc : Api_saisie_read_piqi.node list;
          mutable edges_desc : Api_saisie_read_piqi.edge list;
          mutable nodes_siblings : Api_saisie_read_piqi.node list
        }
      
    end = Graph_tree
and
  Graph_tree_params :
    sig
      type t =
        { mutable index : Api_saisie_read_piqi.protobuf_int32;
          mutable nb_asc : Api_saisie_read_piqi.protobuf_int32 option;
          mutable nb_desc : Api_saisie_read_piqi.protobuf_int32 option
        }
      
    end = Graph_tree_params
  
include Api_saisie_read_piqi
  
let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x
and parse_int64 x = Piqirun.int64_of_zigzag_varint x
and packed_parse_int64 x = Piqirun.int64_of_packed_zigzag_varint x
and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x
and parse_string x = Piqirun.string_of_block x
and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x
and parse_protobuf_int64 x = Piqirun.int64_of_signed_varint x
and packed_parse_protobuf_int64 x = Piqirun.int64_of_packed_signed_varint x
and parse_dmy x =
  let x = Piqirun.parse_record x in
  let (_day, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_month, x) = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let (_year, x) = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let (_delta, x) = Piqirun.parse_required_field 4 parse_protobuf_int32 x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Dmy.day = _day;
       Dmy.month = _month;
       Dmy.year = _year;
       Dmy.delta = _delta;
     })
and parse_date x =
  let x = Piqirun.parse_record x in
  let (_cal, x) = Piqirun.parse_optional_field 2 parse_calendar x in
  let (_prec, x) = Piqirun.parse_optional_field 3 parse_precision x in
  let (_dmy, x) = Piqirun.parse_optional_field 4 parse_dmy x in
  let (_dmy2, x) = Piqirun.parse_optional_field 5 parse_dmy x in
  let (_text, x) = Piqirun.parse_optional_field 6 parse_string x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Date.cal = _cal;
       Date.prec = _prec;
       Date.dmy = _dmy;
       Date.dmy2 = _dmy2;
       Date.text = _text;
     })
and parse_witness_event x =
  let x = Piqirun.parse_record x in
  let (_witness_type, x) =
    Piqirun.parse_required_field 1 parse_witness_type x in
  let (_witness, x) = Piqirun.parse_required_field 2 parse_simple_person x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Witness_event.witness_type = _witness_type;
       Witness_event.witness = _witness;
     })
and parse_event x =
  let x = Piqirun.parse_record x in
  let (_name, x) = Piqirun.parse_required_field 1 parse_string x in
  let (_date, x) = Piqirun.parse_optional_field 2 parse_string x in
  let (_date_conv, x) = Piqirun.parse_optional_field 3 parse_string x in
  let (_date_cal, x) = Piqirun.parse_optional_field 4 parse_calendar x in
  let (_place, x) = Piqirun.parse_optional_field 5 parse_string x in
  let (_reason, x) = Piqirun.parse_optional_field 6 parse_string x in
  let (_note, x) = Piqirun.parse_optional_field 7 parse_string x in
  let (_src, x) = Piqirun.parse_optional_field 8 parse_string x in
  let (_spouse, x) = Piqirun.parse_optional_field 9 parse_simple_person x in
  let (_witnesses, x) = Piqirun.parse_repeated_field 10 parse_witness_event x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Event.name = _name;
       Event.date = _date;
       Event.date_conv = _date_conv;
       Event.date_cal = _date_cal;
       Event.place = _place;
       Event.reason = _reason;
       Event.note = _note;
       Event.src = _src;
       Event.spouse = _spouse;
       Event.witnesses = _witnesses;
     })
and parse_person_tree x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_sex, x) = Piqirun.parse_required_field 2 parse_sex x in
  let (_lastname, x) = Piqirun.parse_required_field 3 parse_string x in
  let (_firstname, x) = Piqirun.parse_required_field 4 parse_string x in
  let (_n, x) = Piqirun.parse_required_field 5 parse_string x in
  let (_p, x) = Piqirun.parse_required_field 6 parse_string x in
  let (_occ, x) = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let (_dates, x) = Piqirun.parse_optional_field 8 parse_string x in
  let (_image, x) = Piqirun.parse_optional_field 9 parse_string x in
  let (_sosa, x) = Piqirun.parse_required_field 10 parse_sosa x in
  let (_has_more_infos, x) = Piqirun.parse_required_field 11 parse_bool x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Person_tree.index = _index;
       Person_tree.sex = _sex;
       Person_tree.lastname = _lastname;
       Person_tree.firstname = _firstname;
       Person_tree.n = _n;
       Person_tree.p = _p;
       Person_tree.occ = _occ;
       Person_tree.dates = _dates;
       Person_tree.image = _image;
       Person_tree.sosa = _sosa;
       Person_tree.has_more_infos = _has_more_infos;
     })
and parse_simple_person x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_sex, x) = Piqirun.parse_required_field 2 parse_sex x in
  let (_lastname, x) = Piqirun.parse_required_field 3 parse_string x in
  let (_firstname, x) = Piqirun.parse_required_field 4 parse_string x in
  let (_n, x) = Piqirun.parse_required_field 5 parse_string x in
  let (_p, x) = Piqirun.parse_required_field 6 parse_string x in
  let (_occ, x) = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let (_birth_short_date, x) =
    Piqirun.parse_optional_field 8 parse_string x in
  let (_birth_place, x) = Piqirun.parse_optional_field 9 parse_string x in
  let (_death_short_date, x) =
    Piqirun.parse_optional_field 10 parse_string x in
  let (_death_place, x) = Piqirun.parse_optional_field 11 parse_string x in
  let (_image, x) = Piqirun.parse_optional_field 12 parse_string x in
  let (_sosa, x) = Piqirun.parse_required_field 13 parse_sosa x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Simple_person.index = _index;
       Simple_person.sex = _sex;
       Simple_person.lastname = _lastname;
       Simple_person.firstname = _firstname;
       Simple_person.n = _n;
       Simple_person.p = _p;
       Simple_person.occ = _occ;
       Simple_person.birth_short_date = _birth_short_date;
       Simple_person.birth_place = _birth_place;
       Simple_person.death_short_date = _death_short_date;
       Simple_person.death_place = _death_place;
       Simple_person.image = _image;
       Simple_person.sosa = _sosa;
     })
and parse_relation_person x =
  let x = Piqirun.parse_record x in
  let (_r_type, x) = Piqirun.parse_required_field 1 parse_relation_type x in
  let (_person, x) = Piqirun.parse_required_field 2 parse_simple_person x
  in
    (Piqirun.check_unparsed_fields x;
     { Relation_person.r_type = _r_type; Relation_person.person = _person; })
and parse_event_witness x =
  let x = Piqirun.parse_record x in
  let (_event_witness_type, x) =
    Piqirun.parse_required_field 1 parse_string x in
  let (_husband, x) = Piqirun.parse_required_field 2 parse_simple_person x in
  let (_wife, x) = Piqirun.parse_optional_field 3 parse_simple_person x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Event_witness.event_witness_type = _event_witness_type;
       Event_witness.husband = _husband;
       Event_witness.wife = _wife;
     })
and parse_person x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_sex, x) = Piqirun.parse_required_field 2 parse_sex x in
  let (_lastname, x) = Piqirun.parse_required_field 3 parse_string x in
  let (_firstname, x) = Piqirun.parse_required_field 4 parse_string x in
  let (_n, x) = Piqirun.parse_required_field 5 parse_string x in
  let (_p, x) = Piqirun.parse_required_field 6 parse_string x in
  let (_occ, x) = Piqirun.parse_required_field 7 parse_protobuf_int32 x in
  let (_public_name, x) = Piqirun.parse_optional_field 8 parse_string x in
  let (_aliases, x) = Piqirun.parse_repeated_field 9 parse_string x in
  let (_qualifiers, x) = Piqirun.parse_repeated_field 10 parse_string x in
  let (_firstname_aliases, x) =
    Piqirun.parse_repeated_field 11 parse_string x in
  let (_surname_aliases, x) =
    Piqirun.parse_repeated_field 12 parse_string x in
  let (_image, x) = Piqirun.parse_optional_field 13 parse_string x in
  let (_birth_date, x) = Piqirun.parse_optional_field 14 parse_string x in
  let (_birth_date_conv, x) =
    Piqirun.parse_optional_field 15 parse_string x in
  let (_birth_date_cal, x) =
    Piqirun.parse_optional_field 16 parse_calendar x in
  let (_birth_place, x) = Piqirun.parse_optional_field 17 parse_string x in
  let (_birth_src, x) = Piqirun.parse_optional_field 18 parse_string x in
  let (_baptism_date, x) = Piqirun.parse_optional_field 19 parse_string x in
  let (_baptism_date_conv, x) =
    Piqirun.parse_optional_field 20 parse_string x in
  let (_baptism_date_cal, x) =
    Piqirun.parse_optional_field 21 parse_calendar x in
  let (_baptism_place, x) = Piqirun.parse_optional_field 22 parse_string x in
  let (_baptism_src, x) = Piqirun.parse_optional_field 23 parse_string x in
  let (_death_date, x) = Piqirun.parse_optional_field 24 parse_string x in
  let (_death_date_conv, x) =
    Piqirun.parse_optional_field 25 parse_string x in
  let (_death_date_cal, x) =
    Piqirun.parse_optional_field 26 parse_calendar x in
  let (_death_place, x) = Piqirun.parse_optional_field 27 parse_string x in
  let (_death_src, x) = Piqirun.parse_optional_field 28 parse_string x in
  let (_death_type, x) =
    Piqirun.parse_required_field 29 parse_death_type x in
  let (_burial_date, x) = Piqirun.parse_optional_field 30 parse_string x in
  let (_burial_date_conv, x) =
    Piqirun.parse_optional_field 31 parse_string x in
  let (_burial_date_cal, x) =
    Piqirun.parse_optional_field 32 parse_calendar x in
  let (_burial_place, x) = Piqirun.parse_optional_field 33 parse_string x in
  let (_burial_src, x) = Piqirun.parse_optional_field 34 parse_string x in
  let (_occupation, x) = Piqirun.parse_optional_field 35 parse_string x in
  let (_notes, x) = Piqirun.parse_optional_field 36 parse_string x in
  let (_psources, x) = Piqirun.parse_optional_field 37 parse_string x in
  let (_has_sources, x) = Piqirun.parse_required_field 38 parse_bool x in
  let (_titles, x) = Piqirun.parse_repeated_field 39 parse_string x in
  let (_related, x) =
    Piqirun.parse_repeated_field 40 parse_relation_person x in
  let (_rparents, x) =
    Piqirun.parse_repeated_field 41 parse_relation_person x in
  let (_father, x) = Piqirun.parse_optional_field 42 parse_simple_person x in
  let (_mother, x) = Piqirun.parse_optional_field 43 parse_simple_person x in
  let (_families, x) = Piqirun.parse_repeated_field 44 parse_family x in
  let (_sosa, x) = Piqirun.parse_required_field 45 parse_sosa x in
  let (_events, x) = Piqirun.parse_repeated_field 46 parse_event x in
  let (_events_witnesses, x) =
    Piqirun.parse_repeated_field 47 parse_event_witness x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Person.index = _index;
       Person.sex = _sex;
       Person.lastname = _lastname;
       Person.firstname = _firstname;
       Person.n = _n;
       Person.p = _p;
       Person.occ = _occ;
       Person.public_name = _public_name;
       Person.aliases = _aliases;
       Person.qualifiers = _qualifiers;
       Person.firstname_aliases = _firstname_aliases;
       Person.surname_aliases = _surname_aliases;
       Person.image = _image;
       Person.birth_date = _birth_date;
       Person.birth_date_conv = _birth_date_conv;
       Person.birth_date_cal = _birth_date_cal;
       Person.birth_place = _birth_place;
       Person.birth_src = _birth_src;
       Person.baptism_date = _baptism_date;
       Person.baptism_date_conv = _baptism_date_conv;
       Person.baptism_date_cal = _baptism_date_cal;
       Person.baptism_place = _baptism_place;
       Person.baptism_src = _baptism_src;
       Person.death_date = _death_date;
       Person.death_date_conv = _death_date_conv;
       Person.death_date_cal = _death_date_cal;
       Person.death_place = _death_place;
       Person.death_src = _death_src;
       Person.death_type = _death_type;
       Person.burial_date = _burial_date;
       Person.burial_date_conv = _burial_date_conv;
       Person.burial_date_cal = _burial_date_cal;
       Person.burial_place = _burial_place;
       Person.burial_src = _burial_src;
       Person.occupation = _occupation;
       Person.notes = _notes;
       Person.psources = _psources;
       Person.has_sources = _has_sources;
       Person.titles = _titles;
       Person.related = _related;
       Person.rparents = _rparents;
       Person.father = _father;
       Person.mother = _mother;
       Person.families = _families;
       Person.sosa = _sosa;
       Person.events = _events;
       Person.events_witnesses = _events_witnesses;
     })
and parse_family x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_spouse, x) = Piqirun.parse_required_field 2 parse_simple_person x in
  let (_marriage_date, x) = Piqirun.parse_optional_field 3 parse_string x in
  let (_marriage_date_conv, x) =
    Piqirun.parse_optional_field 4 parse_string x in
  let (_marriage_date_cal, x) =
    Piqirun.parse_optional_field 5 parse_calendar x in
  let (_marriage_place, x) = Piqirun.parse_optional_field 6 parse_string x in
  let (_marriage_src, x) = Piqirun.parse_optional_field 7 parse_string x in
  let (_marriage_type, x) =
    Piqirun.parse_required_field 8 parse_marriage_type x in
  let (_divorce_type, x) =
    Piqirun.parse_required_field 9 parse_divorce_type x in
  let (_divorce_date, x) = Piqirun.parse_optional_field 10 parse_string x in
  let (_divorce_date_conv, x) =
    Piqirun.parse_optional_field 11 parse_string x in
  let (_divorce_date_cal, x) =
    Piqirun.parse_optional_field 12 parse_calendar x in
  let (_witnesses, x) =
    Piqirun.parse_repeated_field 13 parse_simple_person x in
  let (_notes, x) = Piqirun.parse_optional_field 14 parse_string x in
  let (_fsources, x) = Piqirun.parse_optional_field 15 parse_string x in
  let (_children, x) = Piqirun.parse_repeated_field 16 parse_simple_person x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Family.index = _index;
       Family.spouse = _spouse;
       Family.marriage_date = _marriage_date;
       Family.marriage_date_conv = _marriage_date_conv;
       Family.marriage_date_cal = _marriage_date_cal;
       Family.marriage_place = _marriage_place;
       Family.marriage_src = _marriage_src;
       Family.marriage_type = _marriage_type;
       Family.divorce_type = _divorce_type;
       Family.divorce_date = _divorce_date;
       Family.divorce_date_conv = _divorce_date_conv;
       Family.divorce_date_cal = _divorce_date_cal;
       Family.witnesses = _witnesses;
       Family.notes = _notes;
       Family.fsources = _fsources;
       Family.children = _children;
     })
and parse_index_person x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x
  in (Piqirun.check_unparsed_fields x; { Index_person.index = _index; })
and parse_node x =
  let x = Piqirun.parse_record x in
  let (_id, x) = Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let (_person, x) = Piqirun.parse_required_field 2 parse_person_tree x in
  let (_ifam, x) = Piqirun.parse_optional_field 3 parse_protobuf_int64 x
  in
    (Piqirun.check_unparsed_fields x;
     { Node.id = _id; Node.person = _person; Node.ifam = _ifam; })
and parse_edge x =
  let x = Piqirun.parse_record x in
  let (_from_node, x) =
    Piqirun.parse_required_field 1 parse_protobuf_int64 x in
  let (_to_node, x) = Piqirun.parse_required_field 2 parse_protobuf_int64 x
  in
    (Piqirun.check_unparsed_fields x;
     { Edge.from_node = _from_node; Edge.to_node = _to_node; })
and parse_graph_tree x =
  let x = Piqirun.parse_record x in
  let (_nodes_asc, x) = Piqirun.parse_repeated_field 1 parse_node x in
  let (_edges_asc, x) = Piqirun.parse_repeated_field 2 parse_edge x in
  let (_nodes_desc, x) = Piqirun.parse_repeated_field 3 parse_node x in
  let (_edges_desc, x) = Piqirun.parse_repeated_field 4 parse_edge x in
  let (_nodes_siblings, x) = Piqirun.parse_repeated_field 5 parse_node x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Graph_tree.nodes_asc = _nodes_asc;
       Graph_tree.edges_asc = _edges_asc;
       Graph_tree.nodes_desc = _nodes_desc;
       Graph_tree.edges_desc = _edges_desc;
       Graph_tree.nodes_siblings = _nodes_siblings;
     })
and parse_graph_tree_params x =
  let x = Piqirun.parse_record x in
  let (_index, x) = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let (_nb_asc, x) = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let (_nb_desc, x) = Piqirun.parse_optional_field 3 parse_protobuf_int32 x
  in
    (Piqirun.check_unparsed_fields x;
     {
       Graph_tree_params.index = _index;
       Graph_tree_params.nb_asc = _nb_asc;
       Graph_tree_params.nb_desc = _nb_desc;
     })
and parse_sosa x =
  match Piqirun.int32_of_signed_varint x with
  | 0l -> `sosa_ref
  | 1l -> `sosa
  | 2l -> `no_sosa
  | x -> Piqirun.error_enum_const x
and packed_parse_sosa x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 0l -> `sosa_ref
  | 1l -> `sosa
  | 2l -> `no_sosa
  | x -> Piqirun.error_enum_const x
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
and parse_relation_type x =
  match Piqirun.int32_of_signed_varint x with
  | 0l -> `rparent_adoption
  | 1l -> `rparent_recognition
  | 2l -> `rparent_candidate_parent
  | 3l -> `rparent_god_parent
  | 4l -> `rparent_foster_parent
  | 5l -> `rchild_adoption
  | 6l -> `rchild_recognition
  | 7l -> `rchild_candidate_parent
  | 8l -> `rchild_god_parent
  | 9l -> `rchild_foster_parent
  | x -> Piqirun.error_enum_const x
and packed_parse_relation_type x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 0l -> `rparent_adoption
  | 1l -> `rparent_recognition
  | 2l -> `rparent_candidate_parent
  | 3l -> `rparent_god_parent
  | 4l -> `rparent_foster_parent
  | 5l -> `rchild_adoption
  | 6l -> `rchild_recognition
  | 7l -> `rchild_candidate_parent
  | 8l -> `rchild_god_parent
  | 9l -> `rchild_foster_parent
  | x -> Piqirun.error_enum_const x
and parse_witness_type x =
  match Piqirun.int32_of_signed_varint x with
  | 0l -> `witness
  | 1l -> `witness_godparent
  | x -> Piqirun.error_enum_const x
and packed_parse_witness_type x =
  match Piqirun.int32_of_packed_signed_varint x with
  | 0l -> `witness
  | 1l -> `witness_godparent
  | x -> Piqirun.error_enum_const x
  
let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x
and gen__int64 code x = Piqirun.int64_to_zigzag_varint code x
and packed_gen__int64 x = Piqirun.int64_to_packed_zigzag_varint x
and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x
and gen__string code x = Piqirun.string_to_block code x
and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x
and gen__protobuf_int64 code x = Piqirun.int64_to_signed_varint code x
and packed_gen__protobuf_int64 x = Piqirun.int64_to_packed_signed_varint x
and gen__dmy code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month =
    Piqirun.gen_required_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Dmy.year in
  let _delta = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Dmy.delta
  in Piqirun.gen_record code [ _day; _month; _year; _delta ]
and gen__date code x =
  let _cal = Piqirun.gen_optional_field 2 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 3 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 5 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 6 gen__string x.Date.text
  in Piqirun.gen_record code [ _cal; _prec; _dmy; _dmy2; _text ]
and gen__witness_event code x =
  let _witness_type =
    Piqirun.gen_required_field 1 gen__witness_type
      x.Witness_event.witness_type in
  let _witness =
    Piqirun.gen_required_field 2 gen__simple_person x.Witness_event.witness
  in Piqirun.gen_record code [ _witness_type; _witness ]
and gen__event code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Event.name in
  let _date = Piqirun.gen_optional_field 2 gen__string x.Event.date in
  let _date_conv =
    Piqirun.gen_optional_field 3 gen__string x.Event.date_conv in
  let _date_cal =
    Piqirun.gen_optional_field 4 gen__calendar x.Event.date_cal in
  let _place = Piqirun.gen_optional_field 5 gen__string x.Event.place in
  let _reason = Piqirun.gen_optional_field 6 gen__string x.Event.reason in
  let _note = Piqirun.gen_optional_field 7 gen__string x.Event.note in
  let _src = Piqirun.gen_optional_field 8 gen__string x.Event.src in
  let _spouse =
    Piqirun.gen_optional_field 9 gen__simple_person x.Event.spouse in
  let _witnesses =
    Piqirun.gen_repeated_field 10 gen__witness_event x.Event.witnesses
  in
    Piqirun.gen_record code
      [ _name; _date; _date_conv; _date_cal; _place; _reason; _note; _src;
        _spouse; _witnesses ]
and gen__person_tree code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person_tree.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person_tree.sex in
  let _lastname =
    Piqirun.gen_required_field 3 gen__string x.Person_tree.lastname in
  let _firstname =
    Piqirun.gen_required_field 4 gen__string x.Person_tree.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Person_tree.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Person_tree.p in
  let _occ =
    Piqirun.gen_required_field 7 gen__protobuf_int32 x.Person_tree.occ in
  let _dates =
    Piqirun.gen_optional_field 8 gen__string x.Person_tree.dates in
  let _image =
    Piqirun.gen_optional_field 9 gen__string x.Person_tree.image in
  let _sosa = Piqirun.gen_required_field 10 gen__sosa x.Person_tree.sosa in
  let _has_more_infos =
    Piqirun.gen_required_field 11 gen__bool x.Person_tree.has_more_infos
  in
    Piqirun.gen_record code
      [ _index; _sex; _lastname; _firstname; _n; _p; _occ; _dates; _image;
        _sosa; _has_more_infos ]
and gen__simple_person code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32 x.Simple_person.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Simple_person.sex in
  let _lastname =
    Piqirun.gen_required_field 3 gen__string x.Simple_person.lastname in
  let _firstname =
    Piqirun.gen_required_field 4 gen__string x.Simple_person.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Simple_person.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Simple_person.p in
  let _occ =
    Piqirun.gen_required_field 7 gen__protobuf_int32 x.Simple_person.occ in
  let _birth_short_date =
    Piqirun.gen_optional_field 8 gen__string x.Simple_person.birth_short_date in
  let _birth_place =
    Piqirun.gen_optional_field 9 gen__string x.Simple_person.birth_place in
  let _death_short_date =
    Piqirun.gen_optional_field 10 gen__string
      x.Simple_person.death_short_date in
  let _death_place =
    Piqirun.gen_optional_field 11 gen__string x.Simple_person.death_place in
  let _image =
    Piqirun.gen_optional_field 12 gen__string x.Simple_person.image in
  let _sosa = Piqirun.gen_required_field 13 gen__sosa x.Simple_person.sosa
  in
    Piqirun.gen_record code
      [ _index; _sex; _lastname; _firstname; _n; _p; _occ; _birth_short_date;
        _birth_place; _death_short_date; _death_place; _image; _sosa ]
and gen__relation_person code x =
  let _r_type =
    Piqirun.gen_required_field 1 gen__relation_type x.Relation_person.r_type in
  let _person =
    Piqirun.gen_required_field 2 gen__simple_person x.Relation_person.person
  in Piqirun.gen_record code [ _r_type; _person ]
and gen__event_witness code x =
  let _event_witness_type =
    Piqirun.gen_required_field 1 gen__string
      x.Event_witness.event_witness_type in
  let _husband =
    Piqirun.gen_required_field 2 gen__simple_person x.Event_witness.husband in
  let _wife =
    Piqirun.gen_optional_field 3 gen__simple_person x.Event_witness.wife
  in Piqirun.gen_record code [ _event_witness_type; _husband; _wife ]
and gen__person code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person.sex in
  let _lastname =
    Piqirun.gen_required_field 3 gen__string x.Person.lastname in
  let _firstname =
    Piqirun.gen_required_field 4 gen__string x.Person.firstname in
  let _n = Piqirun.gen_required_field 5 gen__string x.Person.n in
  let _p = Piqirun.gen_required_field 6 gen__string x.Person.p in
  let _occ = Piqirun.gen_required_field 7 gen__protobuf_int32 x.Person.occ in
  let _public_name =
    Piqirun.gen_optional_field 8 gen__string x.Person.public_name in
  let _aliases = Piqirun.gen_repeated_field 9 gen__string x.Person.aliases in
  let _qualifiers =
    Piqirun.gen_repeated_field 10 gen__string x.Person.qualifiers in
  let _firstname_aliases =
    Piqirun.gen_repeated_field 11 gen__string x.Person.firstname_aliases in
  let _surname_aliases =
    Piqirun.gen_repeated_field 12 gen__string x.Person.surname_aliases in
  let _image = Piqirun.gen_optional_field 13 gen__string x.Person.image in
  let _birth_date =
    Piqirun.gen_optional_field 14 gen__string x.Person.birth_date in
  let _birth_date_conv =
    Piqirun.gen_optional_field 15 gen__string x.Person.birth_date_conv in
  let _birth_date_cal =
    Piqirun.gen_optional_field 16 gen__calendar x.Person.birth_date_cal in
  let _birth_place =
    Piqirun.gen_optional_field 17 gen__string x.Person.birth_place in
  let _birth_src =
    Piqirun.gen_optional_field 18 gen__string x.Person.birth_src in
  let _baptism_date =
    Piqirun.gen_optional_field 19 gen__string x.Person.baptism_date in
  let _baptism_date_conv =
    Piqirun.gen_optional_field 20 gen__string x.Person.baptism_date_conv in
  let _baptism_date_cal =
    Piqirun.gen_optional_field 21 gen__calendar x.Person.baptism_date_cal in
  let _baptism_place =
    Piqirun.gen_optional_field 22 gen__string x.Person.baptism_place in
  let _baptism_src =
    Piqirun.gen_optional_field 23 gen__string x.Person.baptism_src in
  let _death_date =
    Piqirun.gen_optional_field 24 gen__string x.Person.death_date in
  let _death_date_conv =
    Piqirun.gen_optional_field 25 gen__string x.Person.death_date_conv in
  let _death_date_cal =
    Piqirun.gen_optional_field 26 gen__calendar x.Person.death_date_cal in
  let _death_place =
    Piqirun.gen_optional_field 27 gen__string x.Person.death_place in
  let _death_src =
    Piqirun.gen_optional_field 28 gen__string x.Person.death_src in
  let _death_type =
    Piqirun.gen_required_field 29 gen__death_type x.Person.death_type in
  let _burial_date =
    Piqirun.gen_optional_field 30 gen__string x.Person.burial_date in
  let _burial_date_conv =
    Piqirun.gen_optional_field 31 gen__string x.Person.burial_date_conv in
  let _burial_date_cal =
    Piqirun.gen_optional_field 32 gen__calendar x.Person.burial_date_cal in
  let _burial_place =
    Piqirun.gen_optional_field 33 gen__string x.Person.burial_place in
  let _burial_src =
    Piqirun.gen_optional_field 34 gen__string x.Person.burial_src in
  let _occupation =
    Piqirun.gen_optional_field 35 gen__string x.Person.occupation in
  let _notes = Piqirun.gen_optional_field 36 gen__string x.Person.notes in
  let _psources =
    Piqirun.gen_optional_field 37 gen__string x.Person.psources in
  let _has_sources =
    Piqirun.gen_required_field 38 gen__bool x.Person.has_sources in
  let _titles = Piqirun.gen_repeated_field 39 gen__string x.Person.titles in
  let _related =
    Piqirun.gen_repeated_field 40 gen__relation_person x.Person.related in
  let _rparents =
    Piqirun.gen_repeated_field 41 gen__relation_person x.Person.rparents in
  let _father =
    Piqirun.gen_optional_field 42 gen__simple_person x.Person.father in
  let _mother =
    Piqirun.gen_optional_field 43 gen__simple_person x.Person.mother in
  let _families =
    Piqirun.gen_repeated_field 44 gen__family x.Person.families in
  let _sosa = Piqirun.gen_required_field 45 gen__sosa x.Person.sosa in
  let _events = Piqirun.gen_repeated_field 46 gen__event x.Person.events in
  let _events_witnesses =
    Piqirun.gen_repeated_field 47 gen__event_witness
      x.Person.events_witnesses
  in
    Piqirun.gen_record code
      [ _index; _sex; _lastname; _firstname; _n; _p; _occ; _public_name;
        _aliases; _qualifiers; _firstname_aliases; _surname_aliases; _image;
        _birth_date; _birth_date_conv; _birth_date_cal; _birth_place;
        _birth_src; _baptism_date; _baptism_date_conv; _baptism_date_cal;
        _baptism_place; _baptism_src; _death_date; _death_date_conv;
        _death_date_cal; _death_place; _death_src; _death_type; _burial_date;
        _burial_date_conv; _burial_date_cal; _burial_place; _burial_src;
        _occupation; _notes; _psources; _has_sources; _titles; _related;
        _rparents; _father; _mother; _families; _sosa; _events;
        _events_witnesses ]
and gen__family code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32 x.Family.index in
  let _spouse =
    Piqirun.gen_required_field 2 gen__simple_person x.Family.spouse in
  let _marriage_date =
    Piqirun.gen_optional_field 3 gen__string x.Family.marriage_date in
  let _marriage_date_conv =
    Piqirun.gen_optional_field 4 gen__string x.Family.marriage_date_conv in
  let _marriage_date_cal =
    Piqirun.gen_optional_field 5 gen__calendar x.Family.marriage_date_cal in
  let _marriage_place =
    Piqirun.gen_optional_field 6 gen__string x.Family.marriage_place in
  let _marriage_src =
    Piqirun.gen_optional_field 7 gen__string x.Family.marriage_src in
  let _marriage_type =
    Piqirun.gen_required_field 8 gen__marriage_type x.Family.marriage_type in
  let _divorce_type =
    Piqirun.gen_required_field 9 gen__divorce_type x.Family.divorce_type in
  let _divorce_date =
    Piqirun.gen_optional_field 10 gen__string x.Family.divorce_date in
  let _divorce_date_conv =
    Piqirun.gen_optional_field 11 gen__string x.Family.divorce_date_conv in
  let _divorce_date_cal =
    Piqirun.gen_optional_field 12 gen__calendar x.Family.divorce_date_cal in
  let _witnesses =
    Piqirun.gen_repeated_field 13 gen__simple_person x.Family.witnesses in
  let _notes = Piqirun.gen_optional_field 14 gen__string x.Family.notes in
  let _fsources =
    Piqirun.gen_optional_field 15 gen__string x.Family.fsources in
  let _children =
    Piqirun.gen_repeated_field 16 gen__simple_person x.Family.children
  in
    Piqirun.gen_record code
      [ _index; _spouse; _marriage_date; _marriage_date_conv;
        _marriage_date_cal; _marriage_place; _marriage_src; _marriage_type;
        _divorce_type; _divorce_date; _divorce_date_conv; _divorce_date_cal;
        _witnesses; _notes; _fsources; _children ]
and gen__index_person code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32 x.Index_person.index
  in Piqirun.gen_record code [ _index ]
and gen__node code x =
  let _id = Piqirun.gen_required_field 1 gen__protobuf_int64 x.Node.id in
  let _person =
    Piqirun.gen_required_field 2 gen__person_tree x.Node.person in
  let _ifam = Piqirun.gen_optional_field 3 gen__protobuf_int64 x.Node.ifam
  in Piqirun.gen_record code [ _id; _person; _ifam ]
and gen__edge code x =
  let _from_node =
    Piqirun.gen_required_field 1 gen__protobuf_int64 x.Edge.from_node in
  let _to_node =
    Piqirun.gen_required_field 2 gen__protobuf_int64 x.Edge.to_node
  in Piqirun.gen_record code [ _from_node; _to_node ]
and gen__graph_tree code x =
  let _nodes_asc =
    Piqirun.gen_repeated_field 1 gen__node x.Graph_tree.nodes_asc in
  let _edges_asc =
    Piqirun.gen_repeated_field 2 gen__edge x.Graph_tree.edges_asc in
  let _nodes_desc =
    Piqirun.gen_repeated_field 3 gen__node x.Graph_tree.nodes_desc in
  let _edges_desc =
    Piqirun.gen_repeated_field 4 gen__edge x.Graph_tree.edges_desc in
  let _nodes_siblings =
    Piqirun.gen_repeated_field 5 gen__node x.Graph_tree.nodes_siblings
  in
    Piqirun.gen_record code
      [ _nodes_asc; _edges_asc; _nodes_desc; _edges_desc; _nodes_siblings ]
and gen__graph_tree_params code x =
  let _index =
    Piqirun.gen_required_field 1 gen__protobuf_int32
      x.Graph_tree_params.index in
  let _nb_asc =
    Piqirun.gen_optional_field 2 gen__protobuf_int32
      x.Graph_tree_params.nb_asc in
  let _nb_desc =
    Piqirun.gen_optional_field 3 gen__protobuf_int32
      x.Graph_tree_params.nb_desc
  in Piqirun.gen_record code [ _index; _nb_asc; _nb_desc ]
and gen__sosa code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `sosa_ref -> 0l | `sosa -> 1l | `no_sosa -> 2l)
and packed_gen__sosa x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `sosa_ref -> 0l | `sosa -> 1l | `no_sosa -> 2l)
and gen__calendar code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `gregorian -> 0l
     | `julian -> 1l
     | `french -> 2l
     | `hebrew -> 3l)
and packed_gen__calendar x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `gregorian -> 0l
     | `julian -> 1l
     | `french -> 2l
     | `hebrew -> 3l)
and gen__precision code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `sure -> 0l
     | `about -> 1l
     | `maybe -> 2l
     | `before -> 3l
     | `after -> 4l
     | `oryear -> 5l
     | `yearint -> 6l)
and packed_gen__precision x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `sure -> 0l
     | `about -> 1l
     | `maybe -> 2l
     | `before -> 3l
     | `after -> 4l
     | `oryear -> 5l
     | `yearint -> 6l)
and gen__sex code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `male -> 0l | `female -> 1l | `unknown -> 2l)
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `male -> 0l | `female -> 1l | `unknown -> 2l)
and gen__death_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `not_dead -> 0l
     | `dead -> 1l
     | `dead_young -> 2l
     | `dead_dont_know_when -> 3l
     | `dont_know_if_dead -> 4l
     | `of_course_dead -> 5l)
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `not_dead -> 0l
     | `dead -> 1l
     | `dead_young -> 2l
     | `dead_dont_know_when -> 3l
     | `dont_know_if_dead -> 4l
     | `of_course_dead -> 5l)
and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `married -> 0l
     | `not_married -> 1l
     | `engaged -> 2l
     | `no_sexes_check_not_married -> 3l
     | `no_mention -> 4l
     | `no_sexes_check_married -> 5l)
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `married -> 0l
     | `not_married -> 1l
     | `engaged -> 2l
     | `no_sexes_check_not_married -> 3l
     | `no_mention -> 4l
     | `no_sexes_check_married -> 5l)
and gen__divorce_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `not_divorced -> 0l | `divorced -> 1l | `separated -> 2l)
and packed_gen__divorce_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `not_divorced -> 0l | `divorced -> 1l | `separated -> 2l)
and gen__relation_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with
     | `rparent_adoption -> 0l
     | `rparent_recognition -> 1l
     | `rparent_candidate_parent -> 2l
     | `rparent_god_parent -> 3l
     | `rparent_foster_parent -> 4l
     | `rchild_adoption -> 5l
     | `rchild_recognition -> 6l
     | `rchild_candidate_parent -> 7l
     | `rchild_god_parent -> 8l
     | `rchild_foster_parent -> 9l)
and packed_gen__relation_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with
     | `rparent_adoption -> 0l
     | `rparent_recognition -> 1l
     | `rparent_candidate_parent -> 2l
     | `rparent_god_parent -> 3l
     | `rparent_foster_parent -> 4l
     | `rchild_adoption -> 5l
     | `rchild_recognition -> 6l
     | `rchild_candidate_parent -> 7l
     | `rchild_god_parent -> 8l
     | `rchild_foster_parent -> 9l)
and gen__witness_type code x =
  Piqirun.int32_to_signed_varint code
    (match x with | `witness -> 0l | `witness_godparent -> 1l)
and packed_gen__witness_type x =
  Piqirun.int32_to_packed_signed_varint
    (match x with | `witness -> 0l | `witness_godparent -> 1l)
  
let gen_int32 x = gen__int32 (-1) x
  
let gen_int64 x = gen__int64 (-1) x
  
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
  
let gen_string x = gen__string (-1) x
  
let gen_bool x = gen__bool (-1) x
  
let gen_protobuf_int64 x = gen__protobuf_int64 (-1) x
  
let gen_dmy x = gen__dmy (-1) x
  
let gen_date x = gen__date (-1) x
  
let gen_witness_event x = gen__witness_event (-1) x
  
let gen_event x = gen__event (-1) x
  
let gen_person_tree x = gen__person_tree (-1) x
  
let gen_simple_person x = gen__simple_person (-1) x
  
let gen_relation_person x = gen__relation_person (-1) x
  
let gen_event_witness x = gen__event_witness (-1) x
  
let gen_person x = gen__person (-1) x
  
let gen_family x = gen__family (-1) x
  
let gen_index_person x = gen__index_person (-1) x
  
let gen_node x = gen__node (-1) x
  
let gen_edge x = gen__edge (-1) x
  
let gen_graph_tree x = gen__graph_tree (-1) x
  
let gen_graph_tree_params x = gen__graph_tree_params (-1) x
  
let gen_sosa x = gen__sosa (-1) x
  
let gen_calendar x = gen__calendar (-1) x
  
let gen_precision x = gen__precision (-1) x
  
let gen_sex x = gen__sex (-1) x
  
let gen_death_type x = gen__death_type (-1) x
  
let gen_marriage_type x = gen__marriage_type (-1) x
  
let gen_divorce_type x = gen__divorce_type (-1) x
  
let gen_relation_type x = gen__relation_type (-1) x
  
let gen_witness_type x = gen__witness_type (-1) x
  
let piqi =
  [ "\226\202\2304\015api_saisie_read\162\244\146\155\011\030geneweb.api.saisie_read.object\218\244\134\182\012\217\001\138\233\142\251\014\210\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$-\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004year\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005delta\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\003dmy\218\244\134\182\012\224\001\138\233\142\251\014\217\001\210\203\242$&\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003cal\210\171\158\194\006\bcalendar\210\203\242$(\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004prec\210\171\158\194\006\tprecision\210\203\242$!\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003dmy\210\171\158\194\006\003dmy\210\203\242$\"\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004dmy2\210\171\158\194\006\003dmy\210\203\242$%\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\218\164\238\191\004\004date\218\244\134\182\012\133\001\138\233\142\251\014\127\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012witness-type\210\171\158\194\006\012witness-type\210\203\242$/\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007witness\210\171\158\194\006\rsimple-person\218\164\238\191\004\rwitness-event\218\244\134\182\012\216\003\138\233\142\251\014\209\003\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\006string\210\203\242$*\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdate-conv\210\171\158\194\006\006string\210\203\242$+\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-cal\210\171\158\194\006\bcalendar\210\203\242$&\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$.\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006spouse\210\171\158\194\006\rsimple-person\210\203\242$1\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rwitness-event\218\164\238\191\004\005event\218\244\134\182\012\253\003\138\233\142\251\014\246\003\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005dates\210\171\158\194\006\006string\210\203\242$&\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$-\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\014has-more-infos\210\171\158\194\006\004bool\218\164\238\191\004\011person-tree\218\244\134\182\012\240\004\138\233\142\251\014\233\004\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016birth-short-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$1\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016death-short-date\210\171\158\194\006\006string\210\203\242$,\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$&\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$#\232\146\150q\026\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\218\164\238\191\004\rsimple-person\218\244\134\182\012\129\001\138\233\142\251\014{\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006r-type\210\171\158\194\006\rrelation-type\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\rsimple-person\218\164\238\191\004\015relation-person\218\244\134\182\012\183\001\138\233\142\251\014\176\001\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\018event-witness-type\210\171\158\194\006\006string\210\203\242$/\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007husband\210\171\158\194\006\rsimple-person\210\203\242$,\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004wife\210\171\158\194\006\rsimple-person\218\164\238\191\004\revent-witness\218\244\134\182\012\145\018\138\233\142\251\014\138\018\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001n\210\171\158\194\006\006string\210\203\242$\"\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\001p\210\171\158\194\006\006string\210\203\242$,\232\146\150q\014\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\022\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$&\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\006string\210\203\242$+\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\006string\210\203\242$0\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015birth-date-conv\210\171\158\194\006\006string\210\203\242$1\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014birth-date-cal\210\171\158\194\006\bcalendar\210\203\242$,\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tbirth-src\210\171\158\194\006\006string\210\203\242$-\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\006string\210\203\242$2\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017baptism-date-conv\210\171\158\194\006\006string\210\203\242$3\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016baptism-date-cal\210\171\158\194\006\bcalendar\210\203\242$.\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$,\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011baptism-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q0\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\006string\210\203\242$0\232\146\150q2\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015death-date-conv\210\171\158\194\006\006string\210\203\242$1\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014death-date-cal\210\171\158\194\006\bcalendar\210\203\242$,\232\146\150q6\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q8\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdeath-src\210\171\158\194\006\006string\210\203\242$/\232\146\150q:\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$,\232\146\150q<\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\006string\210\203\242$1\232\146\150q>\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016burial-date-conv\210\171\158\194\006\006string\210\203\242$2\232\146\150q@\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015burial-date-cal\210\171\158\194\006\bcalendar\210\203\242$-\232\146\150qB\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$+\232\146\150qD\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nburial-src\210\171\158\194\006\006string\210\203\242$+\232\146\150qF\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$&\232\146\150qH\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$)\232\146\150qJ\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$*\232\146\150qL\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\011has-sources\210\171\158\194\006\004bool\210\203\242$'\232\146\150qN\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\006string\210\203\242$1\232\146\150qP\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\015relation-person\210\203\242$2\232\146\150qR\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-person\210\203\242$.\232\146\150qT\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\rsimple-person\210\203\242$.\232\146\150qV\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\rsimple-person\210\203\242$)\232\146\150qX\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\006family\210\203\242$#\232\146\150qZ\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004sosa\210\171\158\194\006\004sosa\210\203\242$&\232\146\150q\\\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006events\210\171\158\194\006\005event\210\203\242$8\232\146\150q^\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\016events-witnesses\210\171\158\194\006\revent-witness\218\164\238\191\004\006person\218\244\134\182\012\218\006\138\233\142\251\014\211\006\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006spouse\210\171\158\194\006\rsimple-person\210\203\242$.\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\006string\210\203\242$3\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018marriage-date-conv\210\171\158\194\006\006string\210\203\242$4\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017marriage-date-cal\210\171\158\194\006\bcalendar\210\203\242$/\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012marriage-src\210\171\158\194\006\006string\210\203\242$5\232\146\150q\016\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$3\232\146\150q\018\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$-\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\006string\210\203\242$2\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017divorce-date-conv\210\171\158\194\006\006string\210\203\242$3\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016divorce-date-cal\210\171\158\194\006\bcalendar\210\203\242$1\232\146\150q\026\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rsimple-person\210\203\242$&\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005notes\210\171\158\194\006\006string\210\203\242$)\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$0\232\146\150q \152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\rsimple-person\218\164\238\191\004\006family\218\244\134\182\012K\138\233\142\251\014E\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\012index-person\218\244\134\182\012\164\001\138\233\142\251\014\157\001\210\203\242$+\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\002id\210\171\158\194\006\014protobuf-int64\210\203\242$,\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006person\210\171\158\194\006\011person-tree\210\203\242$-\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004ifam\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\004node\218\244\134\182\012|\138\233\142\251\014v\210\203\242$2\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfrom-node\210\171\158\194\006\014protobuf-int64\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007to-node\210\171\158\194\006\014protobuf-int64\218\164\238\191\004\004edge\218\244\134\182\012\255\001\138\233\142\251\014\248\001\210\203\242$(\232\146\150q\002\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tnodes-asc\210\171\158\194\006\004node\210\203\242$(\232\146\150q\004\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\tedges-asc\210\171\158\194\006\004edge\210\203\242$)\232\146\150q\006\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nnodes-desc\210\171\158\194\006\004node\210\203\242$)\232\146\150q\b\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nedges-desc\210\171\158\194\006\004edge\210\203\242$-\232\146\150q\n\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\014nodes-siblings\210\171\158\194\006\004node\218\164\238\191\004\ngraph-tree\218\244\134\182\012\186\001\138\233\142\251\014\179\001\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006nb-asc\210\171\158\194\006\014protobuf-int32\210\203\242$0\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007nb-desc\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\017graph-tree-params\218\244\134\182\012V\138\176\205\197\001P\218\164\238\191\004\004sosa\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bsosa-ref\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004sosa\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007no-sosa\218\244\134\182\012s\138\176\205\197\001m\218\164\238\191\004\bcalendar\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tgregorian\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006julian\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006french\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006hebrew\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\tprecision\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004sure\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005about\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005maybe\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006before\170\183\218\222\005\016\232\146\150q\b\218\164\238\191\004\005after\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006oryear\170\183\218\222\005\018\232\146\150q\012\218\164\238\191\004\007yearint\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012\211\001\138\176\205\197\001\204\001\218\164\238\191\004\rmarriage-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007married\170\183\218\222\005\022\232\146\150q\002\218\164\238\191\004\011not-married\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007engaged\170\183\218\222\005%\232\146\150q\006\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005\021\232\146\150q\b\218\164\238\191\004\nno-mention\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022no-sexes-check-married\218\244\134\182\012h\138\176\205\197\001b\218\164\238\191\004\012divorce-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bdivorced\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tseparated\218\244\134\182\012\131\003\138\176\205\197\001\252\002\218\164\238\191\004\rrelation-type\170\183\218\222\005\027\232\146\150q\000\218\164\238\191\004\016rparent-adoption\170\183\218\222\005\030\232\146\150q\002\218\164\238\191\004\019rparent-recognition\170\183\218\222\005#\232\146\150q\004\218\164\238\191\004\024rparent-candidate-parent\170\183\218\222\005\029\232\146\150q\006\218\164\238\191\004\018rparent-god-parent\170\183\218\222\005 \232\146\150q\b\218\164\238\191\004\021rparent-foster-parent\170\183\218\222\005\026\232\146\150q\n\218\164\238\191\004\015rchild-adoption\170\183\218\222\005\029\232\146\150q\012\218\164\238\191\004\018rchild-recognition\170\183\218\222\005\"\232\146\150q\014\218\164\238\191\004\023rchild-candidate-parent\170\183\218\222\005\028\232\146\150q\016\218\164\238\191\004\017rchild-god-parent\170\183\218\222\005\031\232\146\150q\018\218\164\238\191\004\020rchild-foster-parent\218\244\134\182\012R\138\176\205\197\001L\218\164\238\191\004\012witness-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007witness\170\183\218\222\005\028\232\146\150q\002\218\164\238\191\004\017witness-godparent" ]
  


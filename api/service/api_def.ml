(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)

open Def

type ('a, 'b) pb_person =
  | PLight of 'a
  | PFull of 'b
;;

type 'a pb_family =
  | FFull of 'a
;;

type filters =
  { only_sosa : bool;
    only_recent : bool;
    filter_sex : Def.sex option;
    nb_results : bool;
    date_birth : (dmy * dmy * bool) option;
    date_death : (dmy * dmy * bool) option;
  }
;;


module StringSetAutoComplete =
  Set.Make
    (struct
      type t = string ;;
      let compare = compare ;;
     end)
;;

module PlaceSetAutoComplete =
  Set.Make
    (struct
      type t = Def.place ;;
      let compare p1 p2 =
        if compare p1.town p2.town = 0 then
          if compare p1.township p2.township = 0 then
            if compare p1.canton p2.canton = 0 then
              if compare p1.district p2.district = 0 then
                if compare p1.county p2.county = 0 then
                  if compare p1.region p2.region = 0 then
                    if compare p1.country p2.country = 0 then
                      compare p1.other p2.other
                    else compare p1.country p2.country
                  else compare p1.region p2.region
                else compare p1.county p2.county
              else compare p1.district p2.district
            else compare p1.canton p2.canton
          else compare p1.township p2.township
        else compare p1.town p2.town;;
     end)
;;

type cache_type =
  | Cache_string of (string list)
  | Cache_place of (Def.place list)
;;

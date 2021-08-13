(* Copyright (c) 1998-2007 INRIA *)

open Geneweb.Config
open Gwdb
open Geneweb.Util
open Geneweb.Gutil
open Geneweb.Hutil
open Geneweb.SearchName
open Geneweb.AdvSearchOk
open Geneweb.DateDisplay
open Geneweb.Some
open Geneweb.Output
open Def

module Gwdb = Gwdb
module Util = Geneweb.Util
module Gutil = Geneweb.Gutil
module Hutil = Geneweb.Hutil
module SearchName = Geneweb.SearchName
module AdvSearchOk = Geneweb.AdvSearchOk
module DateDisplay = Geneweb.DateDisplay
module Some = Geneweb.Some
module Output = Geneweb.Output
module Request = Gwd_lib.Request

open Plugin_v7_lib

(* from SearchName *)
type search_type =
    Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let search conf base an search_order specify unknown =
  let rec loop l =
    match l with
      [] -> unknown conf an
    | Sosa :: l ->
        let pl = SearchName.search_by_sosa conf base an in
        begin match pl with
          [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | _ -> loop l
        end
    | Key :: l ->
        let pl = SearchName.search_by_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            Util.record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | Surname :: l ->
        let pl = Some.search_surname conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            Some.search_surname_print conf base unknown an
        end
    | FirstName :: l ->
        let pl = Some.search_first_name conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            Some.search_first_name_print conf base an
        end
    | FullName :: l ->
        let max_answers =
          match p_getint conf.env "max" with
            Some n -> n
          | None -> 100
        in
        let fn = match p_getenv conf.env "p" with
          | Some fn -> fn
          | None -> ""
        in
        let sn = match p_getenv conf.env "n" with
          | Some sn -> sn
          | None -> ""
        in
        let conf = { conf with 
          env = ("first_name", fn) :: ("surname", sn) :: conf.env }
        in
        let (list, len) = AdvSearchOk.advanced_search conf base max_answers in
        let list =
          if len > max_answers then Util.reduce_list max_answers list else list
        in
        begin match list with
        | [] -> (* try again without sn *)
          begin
             let list = SearchName.search_approx_key conf base fn in
            if list = [] then loop l
            else
              begin
                let list =
                  List.fold_left (fun list p ->
                    if Name.lower sn = Name.lower (p_surname base p) then p :: list
                    else list) [] list
                in
                begin match list with
                | [] -> loop l
                | [p] ->
                    record_visited conf (get_iper p); V7_perso.print conf base p
                | pl -> specify conf base an pl
                end
              end
          end
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end        
    | ApproxKey :: l ->
        let pl = SearchName.search_approx_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | PartialKey :: l ->
        let pl = SearchName.search_partial_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); V7_perso.print conf base p
        | pl -> specify conf base an pl
        end
    | DefaultSurname :: _ ->
        Some.search_surname_print conf base unknown an
  in
  loop search_order
(* end SearchName *)

(* from request.ml *)
let specify conf base n pl =
  let title _ = Output.printf conf "%s : %s" n (transl conf "specify") in
  let n = Name.crush_lower n in
  let ptll =
    List.map
      (fun p ->
         let tl = ref [] in
         let add_tl t =
           tl :=
             let rec add_rec =
               function
                 t1 :: tl1 ->
                 if eq_istr t1.t_ident t.t_ident &&
                    eq_istr t1.t_place t.t_place
                 then
                   t1 :: tl1
                 else t1 :: add_rec tl1
               | [] -> [t]
             in
             add_rec !tl
         in
         let compare_and_add t pn =
           let pn = sou base pn in
           if Name.crush_lower pn = n then add_tl t
           else
             match get_qualifiers p with
               nn :: _ ->
               let nn = sou base nn in
               if Name.crush_lower (pn ^ " " ^ nn) = n then add_tl t
             | _ -> ()
         in
         List.iter
           (fun t ->
              match t.t_name, get_public_name p with
                Tname s, _ -> compare_and_add t s
              | _, pn when sou base pn <> "" -> compare_and_add t pn
              | _ -> ())
           (nobtit conf base p);
         p, !tl)
      pl
  in
  Hutil.header conf title;
  Hutil.print_link_to_welcome conf true;
  (* Si on est dans un calcul de parenté, on affiche *)
  (* l'aide sur la sélection d'un individu.          *)
  Util.print_tips_relationship conf;
  Output.print_string conf "<ul>\n";
  (* Construction de la table des sosa de la base *)
  let () = V7_sosa.build_sosa_ht conf base in
  List.iter
    (fun (p, tl) ->
       Output.print_string conf "<li>\n";
       V7_sosa.print_sosa conf base p true;
       begin match tl with
           [] ->
           Output.printf conf "\n%s" (referenced_person_title_text conf base p)
         | t :: _ ->
           Output.printf conf "<a href=\"%s%s\">\n" (commd conf)
             (acces conf base p);
           Output.print_string conf (titled_person_text conf base p t);
           Output.print_string conf "</a>\n";
           List.iter
             (fun t -> Output.print_string conf (one_title_text base t)) tl
       end;
       Output.print_string conf (DateDisplay.short_dates_text conf base p);
       if authorized_age conf base p then
         begin match get_first_names_aliases p with
             [] -> ()
           | fnal ->
             Output.print_string conf "\n<em>(";
             Mutil.list_iter_first
               (fun first fna ->
                  if not first then Output.print_string conf ", ";
                  Output.print_string conf (sou base fna))
               fnal;
             Output.print_string conf ")</em>"
         end;
       begin let spouses =
               Array.fold_right
                 (fun ifam spouses ->
                    let cpl = foi base ifam in
                    let spouse = pget conf base (Gutil.spouse (get_iper p) cpl) in
                    if p_surname base spouse <> "?" then spouse :: spouses
                    else spouses)
                 (get_family p) []
         in
         match spouses with
           [] -> ()
         | h :: hl ->
           let s =
             List.fold_left
               (fun s h -> s ^ ",\n" ^ person_title_text conf base h)
               (person_title_text conf base h) hl
           in
           Output.printf conf ", <em>&amp; %s</em>\n" s
       end;
       Output.print_string conf "</li>\n")
    ptll;
  Output.print_string conf "</ul>\n";
  Hutil.trailer conf

let unknown = begin fun conf n ->
      let title _ =
        Output.printf conf "%s: \"%s\"" (Utf8.capitalize_fst (transl conf "not found"))
          (Util.escape_html n)
      in
      Output.status conf Def.Not_Found;
      Hutil.rheader conf title;
      Hutil.print_link_to_welcome conf false;
      Hutil.trailer conf
    end
(* end from request.ml *)

let w_base =
  Request.w_base
    ~none:(fun c -> Gwd_lib.Request.incorrect_request c ; true)
let w_person =
  Request.w_person
    ~none:(fun c b -> Gwd_lib.Request.very_unknown c b ; true)

let home conf base : bool = false

let p =
  w_base begin fun conf base -> match Util.p_getenv conf.env "v" with
    | Some v -> V7_some.first_name_print conf base v ; true
    | None -> false
  end

let s =
  w_base begin fun conf base ->
    let real_input label =
      match p_getenv conf.env label with
        Some s -> if s = "" then None else Some s
      | None -> None
    in
    begin match real_input "p", real_input "n" with
    Some fn, Some sn ->
      let order = [FullName] in
      search conf base (fn ^ " " ^ sn) order specify unknown;
      true
    | Some fn, None ->
      let order =
      [Sosa; Key; FirstName; ApproxKey; PartialKey; DefaultSurname]
      in
      search conf base fn order specify unknown;
      true
    | None, Some sn ->
      let order =
      [Sosa; Key; Surname; ApproxKey; PartialKey; DefaultSurname]
      in
      search conf base sn order specify unknown;
      true
    | None, None -> false
    end
  end

let ns = "welcome"

let _ =
  let aux fn assets conf base =
    Secure.add_assets assets ;
    fn conf base
  in
  Gwd_lib.GwdPlugin.register ~ns
    [ "", aux home
    ; "S", aux s
    ]

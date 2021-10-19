open Geneweb
open Config
open Gwdb
open Def

module A = V8_HTML.ATTR
module T = V8_HTML.TAG

let logo =
  T.pre
    [ "  _____              __          __  _     \n"
      ^ " / ____|             \\ \\        / / | |    \n"
      ^ "| |  __  ___ _ __   __\\ \\  /\\  / /__| |__  \n"
      ^ "| | |_ |/ _ \\ '_ \\ / _ \\ \\/  \\/ / _ \\ '_ \\ \n"
      ^ "| |__| |  __/ | | |  __/\\  /\\  /  __/ |_) |\n"
      ^ " \\_____|\\___|_| |_|\\___| \\/  \\/ \\___|_.__/ \n"
      |> T.pcdata
    ]

let href conf s = A.href (conf.command ^ "?b=" ^ conf.bname ^ s)

module DATE = struct

  let death_symbol conf =
    try List.assoc "death_symbol" conf.base_env
    with Not_found -> "†"
  
  let short conf = function
    | Dtext t ->
      T.pcdata @@ " (" ^ Util.escape_html t ^ ")"
    | Dgreg (d, _) ->
      let s =
        if d.month = 0 then string_of_int d.year
        else if d.day = 0 then string_of_int d.month ^ "/" ^ string_of_int d.year
        else string_of_int d.day ^ "/" ^ string_of_int d.month ^ "/" ^ string_of_int d.year
      in
      T.pcdata @@
      match d.prec with
      | Sure -> s
      | About -> "~" ^ s
      | Before -> "<" ^ s
      | After -> ">" ^ s
      | Maybe -> "?" ^ s
      | OrYear d2 -> s ^ "|" ^ string_of_int d2.year2
      | YearInt d2 -> s ^ ".." ^ string_of_int d2.year2

  let repr_of_date = V8_DATE.repr_of_date
  
end

module IND = struct

  let years conf base p =
    let (birth_date, death_date, _) = Gutil.get_birth_death_date p in
    let y d = Dgreg ({ d with day = 0 ; month = 0 }, Dgregorian) in
    match birth_date, death_date with
    | Some (Dgreg (b, _)), Some (Dgreg (d, _)) ->
      [ DATE.short conf (y b)
      ; T.pcdata "-"
      ; DATE.short conf (y d)
      ]
    | Some (Dgreg (b, _)), _ ->
      begin match get_death p with
        | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
          [ DATE.short conf (y b)
          ; T.pcdata "-"
          ; T.pcdata (DATE.death_symbol conf)
          ]
        | _ -> [ DATE.short conf (y b) ]
      end
    | _, Some (Dgreg (d, _)) ->
      [ T.pcdata (DATE.death_symbol conf)
      ; DATE.short conf (y d)
      ]
    | _, _ -> []

  let designation ?(link = false) conf base p =
    let fn = get_first_name p |> sou base in
    let sn = get_surname p |> sou base in
    if link
    then
      [ T.a ~attr:[ href conf ("&m=P&T=A&v=" ^ Mutil.encode fn) ] [ T.pcdata fn ]
      ; T.pcdata " "
      ; T.a ~attr:[ href conf ("&m=N&T=A&v=" ^ Mutil.encode sn) ] [ T.pcdata sn ]
      ]  
    else
      [ T.pcdata fn ; T.pcdata " " ; T.pcdata sn ]

  
  let sex =
    let aux s = T.span ~attr:[ A.class_ s ] [] in
    function
    | Male -> aux "sex-M"
    | Female -> aux "sex-F"
    | Neuter -> aux "sex-N"

  let ind ?(link = true) ?(dates = true) ?sex:(s = false) conf base p =
    begin fun acc ->
      if s then sex (get_sex p) :: acc
      else acc
    end @@ begin fun acc ->
      let name = designation conf base p in
      if link then T.a ~attr:[ get_iper p
                               |> string_of_iper
                               |> Mutil.encode
                               |> (fun i -> "&i=" ^ i)
                               |> href conf
                             ] name :: acc
      else T.span name :: acc
    end @@ begin
      if dates then
        [ T.pcdata " ("
        ; T.span (years conf base p)
        ; T.pcdata ")" ]
      else []
    end

end

module FAM = struct

  let relation_txt conf sex fam =
    match get_relation fam with
    | NotMarried
    | NoSexesCheckNotMarried ->
      "v8_txt_NotMarried"
    | MarriageBann
    | MarriageContract
    | MarriageLicense
    | Married
    | NoSexesCheckMarried -> "v8_txt_Marriage"
    | Engaged ->
      "v8_txt_Engagement"
    | Pacs ->
      "v8_txt_PACS"
    | Residence
    | NoMention ->
      "v8_txt_NoMention"

  let children conf base ind fam =
    match get_children fam with
    | [||] -> []
    | arr ->
      List.map begin fun iper ->
        let p = poi base iper in
        T.li (ind conf base p)
      end (Array.to_list arr)

  let union conf base p fam =
    let iper = get_iper p in
    let details =
      let place = get_marriage_place fam in
      let date = get_marriage fam in
      let date =
        if date = Adef.cdate_None then ""
        else DATE.repr_of_date (Adef.date_of_cdate date)
      in
      let place =
        if is_empty_string place then ""
        else sou base place
      in
      if date = "" then if place = "" then "" else place ^ " "
      else if place = "" then date ^ " "
      else place ^ " - " ^ date ^ " "
    in
    let relation = T.pcdata (" " ^ relation_txt conf (get_sex p) fam ^ " ") in
    begin fun acc ->
      if details = "" then relation :: acc
      else T.span ~attr:[ A.tabindex "0" ] [ relation ]
           :: T.span ~attr:[ A.class_ "details" ] [ T.pcdata details ]
           :: acc
    end [ T.span (IND.ind conf base (Gutil.spouse iper fam |> poi base)) ]
  
end


module FORM = struct

  let extra ?(txt = [ T.pcdata "+"  ]) i acc =
    T.input ~attr:A.[ type_ "checkbox" ; class_ "checkbox-extra" ; id ("checkbox-extra-" ^ i) ] ()
    :: T.label ~attr:A.[ class_ "button" ; for_ ("checkbox-extra-" ^ i) ] txt
    :: acc

end

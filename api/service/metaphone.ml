(*pp camlp4o -I `ocamlfind query piqi.syntax` pa_labelscope.cmo pa_openin.cmo *)


let slavo_Germanic sstring =
  let rec loop i =
    if i = String.length sstring then false
    else
      match (String.sub sstring i 1) with
      | "W" | "K" -> true
      | "C" -> 
          if (i+1) < String.length sstring then
            if (String.sub sstring (i+1) 1) = "Z" then true else loop (i+1)
          else false
      | _ -> loop (i + 1)
  in
  loop 0
(*  List.mem sstring ["W"; "K"; "CZ"; "WITZ"]*) (* !!!! *)
;;


let is_vowel sstring pos =
  if pos < 0 then false
  else List.mem (String.sub sstring pos 1) ["A"; "E"; "I"; "O"; "U"; "Y"]
;;


let string_at sstring start length list =
  let ret = ref false in
    try
      if ((start < 0) || (start >= String.length(sstring))) then !ret
      else
        begin
          for i = 0 to Array.length list - 1 do
            if (list.(i) = String.sub sstring start length) then 
              begin 
                ret:= true; 
                raise Exit; 
              end
            else ()
          done;
          !ret
        end
    with Exit -> !ret
;;

let double_metaphone sstring =
  let primary   = ref "" in
  let secondary = ref "" in
  let current   = ref 0 in
  	
  let length   = String.length (sstring) in
  let last     = length - 1 in
  let original = sstring ^ "     " in

  let original = String.uppercase original in

(*    // skip this at beginning of word*)
    
  if (string_at original 0 2 [|"GN"; "KN"; "PN"; "WR"; "PS"|]) then incr current;

(*    // Initial "X" is pronounced "Z" e.g. "Xavier"*)
    
    if (String.sub original 0 1 = "X") then
    begin
      primary   := !primary ^ "S";  (* // "Z" maps to "S"*)
      secondary := !secondary ^ "S";
      incr current;
    end;

(try
(*    // main loop*)
    while (String.length !primary < 4 || String.length !secondary < 4) do
      if (!current >= length) then raise Exit;
      
      begin
(try
      match (String.sub original !current 1) with
      | "A"
      | "E"
      | "I"
      | "O"
      | "U"
      | "Y" ->
          if (!current = 0) then begin
            (*// all init vowels now map to "A"*)
            primary   := !primary ^ "A";
            secondary := !secondary ^ "A";
          end;
          incr current;

      | "B" ->
(*          // "-mb", e.g. "dumb", already skipped over ...*)
          primary   := !primary ^ "P";
          secondary := !secondary ^ "P";

          if String.sub original (!current + 1) 1 = "B" then
            begin incr current; incr current; end
          else  incr current;


        | "Ç" ->
          primary   := !primary ^ "S";
          secondary := !secondary ^ "S";

        | "C" ->
(*          // various gremanic*)
          if ((!current > 1) 
              && not (is_vowel original (!current - 2))
              && string_at original (!current - 1) 3 [| "ACH" |]
              && ((String.sub original (!current + 2)  1 <> "I")
                  && ((String.sub original (!current + 2)  1 <> "E")
                      || string_at original (!current - 2) 6 [|"BACHER"; "MACHER"|]))) then begin
          primary   := !primary ^ "K";
          secondary := !secondary ^ "K";
          incr current;
          incr current;
          raise Exit
          end;

(*          // special case "caesar"*)
          if ((!current = 0) 
              && string_at original !current 6 [|"CAESAR"|]) then begin
          primary   := !primary ^ "S";
          secondary := !secondary ^ "S";
          incr current;
          incr current;
          raise Exit
          end;

(*          // italian "chianti"*)
          if (string_at original !current 4 [|"CHIA"|]) then begin
          primary   := !primary ^ "K";
          secondary := !secondary ^ "K";
          incr current;
          incr current;
          raise Exit
          end;

          if (string_at original !current 2 [|"CH"|]) then begin

(*            // find "michael"*)
            if ((!current > 0)  && string_at original !current 4 [|"CHAE"|]) then begin
              primary   := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current;
              incr current;
              raise Exit
            end;

(*            // greek roots e.g. "chemistry", "chorus"*)
            if ((!current = 0)
                && (string_at original (!current + 1) 5 [|"HARAC"; "HARIS"|])
                    || string_at original (!current + 1) 3 [|"HOR"; "HYM"; "HIA"; "HEM"|])
                && not (string_at original 0 5 [|"CHORE"|]) then begin
                     primary   := !primary ^ "K";
                     secondary := !secondary ^ "K";
                     incr current;
                     incr current;
                     raise Exit
                end;

(*            // germanic, greek, or otherwise "ch" for "kh" sound*)
            if ((string_at original 0 4 [|"VAN "; "VON "|])
                 || string_at original 0 3 [|"SCH"|])
(*                // "architect" but not "arch", orchestra, "orchid"*)
                || string_at original (!current - 2) 6 [|"ORCHES"; "ARCHIT"; "ORCHID"|]
                || string_at original (!current + 2) 1 [|"T"; "S"|]
                || ((string_at original (!current - 1) 1 [|"A";"O";"U";"E"|])
                     || (!current = 0))
(*                    // e.g. "wachtler", "weschsler", but not "tichner"*)
                    && string_at original (!current + 2) 1 [|"L";"R";"N";"M";"B";"H";"F";"V";"W";" "|] then begin
                      primary   := !primary ^ "K";
                      secondary := !secondary ^ "K";
                    end 
            else begin
              if (!current > 0) then begin
                if string_at original 0 2 [|"MC"|] then begin
(*                  // e.g. "McHugh"*)
                  primary := !primary ^ "K";
                  secondary := !secondary ^ "K";
                end else begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "K";
                end
              end else begin
                primary := !primary ^ "X";
                secondary := !secondary ^ "X";
              end
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // e.g. "czerny"*)
          if string_at original !current 2 [|"CZ"|] && 
            not (string_at original (!current - 2) 4 [|"WICZ"|]) then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "X";
            incr current; incr current;
            raise Exit
            end;

(*          // e.g. "focaccia"*)
          if string_at original (!current + 1) 3 [|"CIA"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
          end;
(*          // double "C", but not McClellan*)
          if string_at original !current 2 [|"CC"|]
              && not ((!current = 1) 
                   && (String.sub original 0 1) = "M") then begin
(*            // "bellocchio" but not "bacchus"*)
            if string_at original (!current + 2) 1                       [|"I";"E";"H"|]
                && not (string_at original (!current + 2) 2                          [|"HU"|]) then begin
(*              // "accident", "accede", "succeed"*)
              if (((!current = 1)
                   && (String.sub original (!current - 1) 1) = "A"))
                  || string_at original (!current - 1) 5                            [|"UCCEE"; "UCCES"|] then begin
                primary := !primary ^ "KS";
                secondary := !secondary ^ "KS";
(*                // "bacci", "bertucci", other italian*)
              end else begin
                primary := !primary ^ "X";
                secondary := !secondary ^ "X";
              end;
              incr current; incr current; incr current;
              raise Exit
            end else begin
(*              // Pierce's rule*)
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current; incr current;
              raise Exit
            end
              end;

          if string_at original  !current 2 [|"CK";"CG";"CQ"|] then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "K";
            incr current; incr current;
            raise Exit
          end;

          if string_at original  !current 2 [|"CI";"CE";"CY"|] then begin
(*            // italian vs. english*)
            if string_at original  !current 3  [|"CIO";"CIE";"CIA"|] then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "X";
            end else begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // else*)
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";


(*          // name sent in "mac caffrey", "mac gregor"*)
          if string_at original  (!current + 1) 2 [|" C";" Q";" G"|] then begin
            incr current; incr current; incr current;
          end else begin
            if string_at original  (!current + 1) 1  [|"C";"K";"Q"|]
                && not (string_at original (!current + 1) 2 [|"CE";"CI"|]) then begin
              incr current; incr current;
            end else begin
              incr current;
            end
          end;
          raise Exit;

        | "D" ->
          if string_at original  !current 2    [|"DG"|] then begin
            if string_at original  (!current + 2) 1          [|"I";"E";"Y"|] then begin
(*              // e.g. "edge"*)
              primary := !primary ^ "J";
              secondary := !secondary ^ "J";
              incr current; incr current; incr current;
              raise Exit
            end else begin
(*              // e.g. "edgar"*)
              primary := !primary ^ "TK";
              secondary := !secondary ^ "TK";
              incr current; incr current;
              raise Exit
            end
          end;

          if string_at original  !current 2   [|"DT";"DD"|] then begin
            primary := !primary ^ "T";
            secondary := !secondary ^ "T";
            incr current; incr current;
            raise Exit
          end;

(*          // else*)
          primary := !primary ^ "T";
          secondary := !secondary ^ "T";
          incr current;
          raise Exit

        | "F" ->
          if (String.sub original (!current + 1) 1) = "F" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "F";
          secondary := !secondary ^ "F";
          raise Exit

        | "G" ->
          if (String.sub original (!current + 1) 1) = "H" then begin
            if (!current > 0) 
                && not (is_vowel original (!current - 1)) then begin
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
              incr current; incr current;
              raise Exit
            end;

            if (!current < 3) then begin
(*              // "ghislane", "ghiradelli"*)
              if (!current = 0) then begin
                if (String.sub original (!current + 2) 1) = "I" then begin
                  primary := !primary ^ "J";
                  secondary := !secondary ^ "J";
                end else begin
                  primary := !primary ^ "K";
                  secondary := !secondary ^ "K";
                end;
                incr current; incr current;
                raise Exit
              end
            end;

(*            // Parker's rule (with some further refinements) - e.g. "hugh"*)
            if (((!current > 1)
                 && string_at original (!current - 2) 1                  [|"B";"H";"D"|])
(*                // e.g. "bough"*)
                || ((!current > 2)
                    && string_at original (!current - 3) 1                 [|"B";"H";"D"|])
(*                // e.g. "broughton"*)
                || ((!current > 3)
                    && string_at original (!current - 4) 1    [|"B";"H"|])) then begin
              incr current; incr current;
              raise Exit
            end else begin
(*              // e.g. "laugh", "McLaughlin", "cough", "gough", "rough", "tough"*)
              if ((!current > 2)
                  && (String.sub original (!current - 1) 1) = "U")
                  && string_at original (!current - 3) 1  [|"C";"G";"L";"R";"T"|] then begin
                primary := !primary ^ "F";
                secondary := !secondary ^ "F";
              end else if (!current > 0)
                        && String.sub original (!current - 1) 1 <> "I" then begin
                primary := !primary ^ "K";
                secondary := !secondary ^ "K";
              end;
              incr current; incr current;
              raise Exit
            end
          end;

          if (String.sub original (!current + 1) 1) = "N" then begin
            if ((!current = 1) && is_vowel original 0
                && not (slavo_Germanic original)) then begin
              primary := !primary ^ "KN";
              secondary := !secondary ^ "N";
            end else begin
(*              // not e.g. "cagney"*)
              if not (string_at original (!current + 2) 2  [|"EY"|])
                  && (String.sub original (!current + 1) (String.length original - (!current + 1))) <> "Y"
                  && not (slavo_Germanic original) then begin
                 primary := !primary ^ "N";
                 secondary := !secondary ^ "KN";
              end else begin
                 primary := !primary ^ "KN";
                 secondary := !secondary ^ "KN";
              end
            end;
            incr current; incr current;
            raise Exit
          end;

(*          // "tagliaro"*)
          if string_at original  (!current + 1) 2 [|"LI"|]
              && not (slavo_Germanic original) then begin
            primary := !primary ^ "KL";
            secondary := !secondary ^ "L";
            incr current; incr current;
            raise Exit
          end ;

(*          // -ges- -gep- -gel- at beginning*)
          if ((!current = 0)
              && ((String.sub original (!current + 1) 1) = "Y"
                  || string_at original (!current + 1) 2 [|"ES";"EP";"EB";"EL";"EY";"IB";"IL";"IN";"IE";"EI";"ER"|])) then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
          end;

(*          // -ger- -gy-*)
          if (string_at original  (!current + 1) 2 [|"ER"|]
               || (String.sub original (!current + 1) 1) = "Y")
              && not (string_at original 0 6 [|"DANGER";"RANGER";"MANGER"|])
              && not (string_at original (!current - 1) 1 [|"E"; "I"|])
              && not (string_at original (!current - 1) 3 [|"RGY";"OGY"|]) then begin
            primary := !primary ^ "K";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
                         end;

(*          // italian e.g. "biaggi"*)
          if string_at original  (!current + 1) 1  [|"E";"I";"Y"|]
              || string_at original (!current - 1) 4 [|"AGGI";"OGGI"|] then begin
(*            // obvious germanic*)
            if (string_at original  0 4 [|"VAN "; "VON "|])
                 || string_at original 0 3 [|"SCH"|]
                || string_at original (!current + 1) 2 
                          [|"ET"|] then begin
              primary := !primary ^ "K";
              secondary := !secondary ^ "K";
            end else begin
(*              // always soft if french ending*)
              if string_at original  (!current + 1) 4 [|"IER "|] then begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "J";
              end else begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "K";
              end
            end;
            incr current; incr current;
            raise Exit
                        end;

          if (String.sub original (!current + 1) 1) = "G" then begin
            incr current; incr current;
            end
          else
            incr current;

          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "H" ->
(*          // only keep if first & before vowel or btw. 2 vowels*)
          if (((!current = 0) || 
               is_vowel original (!current - 1))
              && is_vowel original (!current + 1)) then begin
            primary := !primary ^ "H";
            secondary := !secondary ^ "H";
            incr current; incr current;
          end else
            incr current;
          raise Exit

        | "J" ->
          (*  obvious spanish "jose" "san jacinto" *)
          if string_at original  !current 4 [|"JOSE"|]             (* peut etre un not *)
              || string_at original 0 4 [|"SAN "|] then begin
            if (((!current = 0)
                 && (String.sub original (!current + 4) 1) = " "))
                || string_at original 0 4 [|"SAN "|] then begin
              primary := !primary ^ "H";
              secondary := !secondary ^ "H";
            end else begin
              primary := !primary ^ "J";
              secondary := !secondary ^ "H";
            end;
            incr current;
            raise Exit
              end;

          if (!current = 0)
              && not (string_at original !current 4 [|"JOSE"|]) then begin
            primary := !primary ^ "J";  (*  Yankelovich/Jankelowicz *)
            secondary := !secondary ^ "A";
          end else begin
            (*  spanish pron. of .e.g. "bajador" *)
            if (is_vowel original (!current - 1)
                && not (slavo_Germanic original)
                && ((String.sub original (!current + 1) 1) = "A")
                    || (String.sub original (!current + 1) 1) = "O") then begin
              primary := !primary ^ "J";
              secondary := !secondary ^ "H";
            end else begin
              if (!current = last) then begin
                primary := !primary ^ "J";
                secondary := !secondary ^ "";
              end else begin
                if not (string_at original (!current + 1) 1 [|"L";"T";"K";"S";"N";"M";"B";"Z"|])
                    && not (string_at original (!current - 1) 1 [|"S";"K";"L"|]) then begin
                  primary := !primary ^ "J";
                  secondary := !secondary ^ "J";
                end
              end
            end
          end;

          if (String.sub original (!current + 1) 1) = "J" then begin (*  it could happen *)
            incr current; incr current; end
          else 
            incr current;
          raise Exit

        | "K" ->
          if (String.sub original (!current + 1) 1) = "K" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "L" ->
          if (String.sub original (!current + 1) 1) = "L" then begin
            (*  spanish e.g. "cabrillo" "gallegos" *)
            if ((!current = (length - 3))
                 && string_at original (!current - 1) 4 
                           [|"ILLO";"ILLA";"ALLE"|]
                || (string_at original  (last-1) 2 
                            [|"AS";"OS"|])
                  || string_at original last 1 
                            [|"A";"O"|]
                 && string_at original (!current - 1) 4 
                           [|"ALLE"|]) then begin
              primary := !primary ^ "L";
              secondary := !secondary ^ "";
              incr current; incr current;
              raise Exit
            end;
            incr current; incr current;
          end else 
            incr current;
          primary := !primary ^ "L";
          secondary := !secondary ^ "L";
          raise Exit

        | "M" ->
          if (string_at original  (!current - 1) 3 
                     [|"UMB"|])
               && ((((!current + 1)) = last)
                   || string_at original (!current + 2) 2 
                            [|"ER"|])
              (*  "dumb" "thumb" *)
              || (String.sub original (!current + 1) 1) = "M" then begin
              incr current; incr current;
          end else begin
              incr current;
          end;
          primary := !primary ^ "M";
          secondary := !secondary ^ "M";
          raise Exit

        | "N" ->
          if (String.sub original (!current + 1) 1) = "N" then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "N";
          secondary := !secondary ^ "N";
          raise Exit

        | "Ñ" ->
          incr current;
          primary := !primary ^ "N";
          secondary := !secondary ^ "N";
          raise Exit

        | "P" ->
          if (String.sub original (!current + 1) 1) = "H" then begin
            incr current; incr current;
            primary := !primary ^ "F";
            secondary := !secondary ^ "F";
            raise Exit
          end;

          (*  also account for "campbell" and "raspberry" *)
          if string_at original  (!current + 1) 1 
                     [|"P";"B"|] then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "P";
          secondary := !secondary ^ "P";
          raise Exit

        | "Q" ->
          if (String.sub original (!current + 1) 1) = "Q" then begin
            incr current; incr current; end
          else 
            incr current;
          primary := !primary ^ "K";
          secondary := !secondary ^ "K";
          raise Exit

        | "R" ->
          (*  french e.g. "rogier" but exclude "hochmeier" *)
          if ((!current = last)
              && not (slavo_Germanic(original))
              && string_at original (!current - 2) 2  [|"IE"|])
              && not (string_at original (!current - 4) 2  [|"ME";"MA"|]) then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "R";
          end else begin
            primary := !primary ^ "R";
            secondary := !secondary ^ "R";
          end;
          if (String.sub original (!current + 1) 1) = "R" then begin
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "S" ->
          (*  special |s "island" "isle" "carlisle" "carlysle" *)
          if string_at original  (!current - 1) 3 
                     [|"ISL";"YSL"|] then begin
            incr current;
            raise Exit
                     end;

          (*  special | "sugar-" *)
          if (!current = 0)
              && string_at original !current 5  [|"SUGAR"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "S";
            incr current;
            raise Exit
                        end;

          if string_at original  !current 2  [|"SH"|] then begin
            (*  germanic *)
            if string_at original  (!current + 1) 4 
                       [|"HEIM";"HOEK";"HOLM";"HOLZ"|] then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end else begin
              primary := !primary ^ "X";
              secondary := !secondary ^ "X";
            end;
            incr current; incr current;
            raise Exit
                     end;

          (*  italian & armenian  *)
          if string_at original  !current 3  [|"SIO";"SIA"|]
              || string_at original !current 4  [|"SIAN"|] then begin
            if (not(slavo_Germanic(original))) then begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "X";
            end else begin
              primary := !primary ^ "S";
              secondary := !secondary ^ "S";
            end;
            incr current; incr current; incr current;
            raise Exit
                        end;

          (*  german & anglicisations e.g. "smith" match "schmidt" "snider" match "schneider" *)
          (*  also -sz- in slavic language altho in hungarian it is pronounced "s" *)
          if ((!current = 0)
               && string_at original (!current + 1) 1 [|"M";"N";"L";"W"|])
              || string_at original (!current + 1) 1 [|"Z"|] then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "X";
            if string_at original  (!current + 1) 1 
                        [|"Z"|] then begin
              incr current; incr current; end
            else
              incr current;
            raise Exit
                        end;

          if string_at original  !current 2 [|"SC"|] then begin
            (*  Schlesinger's rule  *)
            if (String.sub original (!current + 2) 1) = "H" then begin
              (*  dutch origin e.g. "school" "schooner" *)
              if string_at original  (!current + 3) 2 [|"OO";"ER";"EN";"UY";"ED";"EM"|] then begin
                (*  "schermerhorn" "schenker"  *)
                if string_at original  (!current + 3) 2  [|"ER";"EN"|] then begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "SK";
                end else begin
                  primary := !primary ^ "SK";
                  secondary := !secondary ^ "SK";
                end;
                incr current; incr current; incr current;
                raise Exit
              end else begin
                if ((!current = 0) 
                    && not (is_vowel original 3)
                    && (String.sub original (!current + 3) 1) <> "W") then begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "S";
                end else begin
                  primary := !primary ^ "X";
                  secondary := !secondary ^ "X";
                end;
                incr current; incr current; incr current;
                raise Exit
              end;
            end else begin
              if string_at original  (!current + 2) 1 [|"I";"E";"Y"|] then begin
                primary := !primary ^ "S";
                secondary := !secondary ^ "S";
                incr current; incr current; incr current;
                raise Exit
              end;

            primary := !primary ^ "SK";
            secondary := !secondary ^ "SK";
            incr current; incr current; incr current;
            raise Exit

            end;
          end;

          (*  french e.g. "resnais" "artois" *)
          if (!current = last)
              && string_at original (!current - 2) 2 [|"AI";"OI"|] then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "S";
          end else begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "S";
          end;

          if string_at original  (!current + 1) 1  [|"S";"Z"|] then begin
            incr current; incr current; end
          else 
            incr current;
          raise Exit

        | "T" ->
          if string_at original  !current 4 
                     [|"TION"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
                     end;

          if string_at original  !current 3 
                     [|"TIA";"TCH"|] then begin
            primary := !primary ^ "X";
            secondary := !secondary ^ "X";
            incr current; incr current; incr current;
            raise Exit
                     end;

          if string_at original  !current 2 
                     [|"TH"|]
              || string_at original !current 3 
                            [|"TTH"|] then begin
            (*  special | "thomas" "thames" or germanic *)
            if string_at original  (!current + 2) 2 
                       [|"OM";"AM"|]
                || string_at original 0 4 [|"VAN ";"VON "|]
                || string_at original 0 3 [|"SCH"|] then begin
              primary := !primary ^ "T";
              secondary := !secondary ^ "T";
            end else begin
              primary := !primary ^ "0";
              secondary := !secondary ^ "T";
            end;
            incr current; incr current;
            raise Exit
                            end;

          if string_at original  (!current + 1) 1 
                     [|"T";"D"|] then begin
            incr current; incr current; end
          else
            incr current;
          primary := !primary ^ "T";
          secondary := !secondary ^ "T";
          raise Exit

        | "V" ->
          if (String.sub original (!current + 1) 1) = "V" then begin 
            incr current; incr current;end
          else
            incr current;
          primary := !primary ^ "F";
          secondary := !secondary ^ "F";
          raise Exit

        | "W" ->
          (*  can also be in middle of word *)
          if string_at original  !current 2 [|"WR"|] then begin
            primary := !primary ^ "R";
            secondary := !secondary ^ "R";
            incr current; incr current;
            raise Exit
          end;

          if (!current = 0)
              && (is_vowel original (!current + 1)
                  || string_at original !current 2 
                            [|"WH"|]) then begin
            (*  Wasserman should match Vasserman  *)
            if (is_vowel original (!current + 1)) then begin
              primary := !primary ^ "A";
              secondary := !secondary ^ "F";
            end else begin
              (*  need Uomo to match Womo  *)
              primary := !primary ^ "A";
              secondary := !secondary ^ "A";
            end
              end;

          (*  Arnow should match Arnoff *)
          if (((!current = last) 
                && is_vowel original (!current - 1))
              || string_at original (!current - 1) 5 
                        [|"EWSKI";"EWSKY";"OWSKI";"OWSKY"|]
              || string_at original 0 3 [|"SCH"|]) then begin
            primary := !primary ^ "";
            secondary := !secondary ^ "F";
            incr current;
            raise Exit
          end;

          (*  polish e.g. "filipowicz" *)
          if string_at original  !current 4 
                     [|"WICZ";"WITZ"|] then begin
            primary := !primary ^ "TS";
            secondary := !secondary ^ "FX";
            incr current; incr current; incr current; incr current;
            raise Exit
                     end;

          (*  else skip it *)
          incr current;
          raise Exit

        | "X" ->
          (*  french e.g. breaux  *)
          if (not ((!current = last)
                && (string_at original  (!current - 3) 3 [|"IAU"; "EAU"|]
                 || string_at original (!current - 2) 2 [|"AU"; "OU"|]))) then begin
            primary := !primary ^ "KS";
            secondary := !secondary ^ "KS";
          end;

          if string_at original  (!current + 1) 1  [|"C";"X"|] then begin
            incr current; incr current; end
          else
            incr current;
          raise Exit

        | "Z" ->
          (*  chinese pinyin e.g. "zhao"  *)
          if (String.sub original (!current + 1) 1) = "H" then begin
            primary := !primary ^ "J";
            secondary := !secondary ^ "J";
            incr current; incr current;
            raise Exit
          end else if string_at original  (!current + 1) 2  [|"ZO"; "ZI"; "ZA"|]
                    || (slavo_Germanic(original)
                        && ((!current > 0)
                            && String.sub original (!current - 1) 1 <> "T")) then begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "TS";
          end else begin
            primary := !primary ^ "S";
            secondary := !secondary ^ "S";
          end;

          if (String.sub original (!current + 1) 1) = "Z" then begin
            incr current; incr current;end
          else
            incr current;
          raise Exit

        | _ ->
          incr current;

with Exit -> ())
    end; (*  end switch *)

(*
    print_endline original;
    print_endline (string_of_int !current);
    print_endline (!primary);
    print_endline (!secondary);
*)

    done; (*(*  end while*) *)
with Exit -> ());
  
(*
    if String.length !primary > 4 then
      primary   := String.sub !primary   0 4;
    if String.length !secondary > 4 then
    secondary := String.sub !secondary 0 4;

    print_endline original;
    print_endline (!primary);
    print_endline (!secondary);
    
    if( !primary = !secondary ) then begin
    	secondary := "" 
    end;
*)

    (!primary, !secondary)

(*    
    $result["primary"] = $primary ;
    $result["secondary"] = $secondary ;
*)

(*    
    return $result ;
*)

(*  end (*  end of function MetaPhone*) *)
;;  
  

(*
double_metaphone "LHEUREUX";print_endline " ";
double_metaphone "L'HEUREUX";print_endline " ";
double_metaphone "LHEREX";print_endline " ";

double_metaphone "DUPONT";print_endline " ";
double_metaphone "DUPOND";print_endline " ";
double_metaphone "DUPPON";print_endline " ";
*)

(*
let (p, s) = double_metaphone ("GALLICHON") in print_endline (p ^ " " ^ s);
let (p, s) = double_metaphone ("GALICHON") in print_endline (p ^ " " ^ s);
let (p, s) = double_metaphone ("GALLICON") in print_endline (p ^ " " ^ s);
let (p, s) = double_metaphone ("GALICON") in print_endline (p ^ " " ^ s);
*)

(*
print_endline (string_of_bool (double_metaphone "ALLERTON" = "ALRT"));
print_endline (string_of_bool (double_metaphone "Acton" = "AKTN"));
print_endline (string_of_bool (double_metaphone "Adams" = "ATMS"));
print_endline (string_of_bool (double_metaphone "Aggar" = "AKR"));
print_endline (string_of_bool (double_metaphone "Ahl" = "AL"));
print_endline (string_of_bool (double_metaphone "Aiken" = "AKN"));
print_endline (string_of_bool (double_metaphone "Alan" = "ALN"));
print_endline (string_of_bool (double_metaphone "Alcock" = "ALKK"));
print_endline (string_of_bool (double_metaphone "Alden" = "ALTN"));
print_endline (string_of_bool (double_metaphone "Aldham" = "ALTM"));
print_endline (string_of_bool (double_metaphone "Allen" = "ALN"));
print_endline (string_of_bool (double_metaphone "Allerton" = "ALRT"));
print_endline (string_of_bool (double_metaphone "Alsop" = "ALSP"));
print_endline (string_of_bool (double_metaphone "Alwein" = "ALN"));
print_endline (string_of_bool (double_metaphone "Ambler" = "AMPL"));
print_endline (string_of_bool (double_metaphone "Andevill" = "ANTF"));
print_endline (string_of_bool (double_metaphone "Andrews" = "ANTR"));
print_endline (string_of_bool (double_metaphone "Andreyco" = "ANTR"));
print_endline (string_of_bool (double_metaphone "Andriesse" = "ANTR"));
print_endline (string_of_bool (double_metaphone "Angier" = "ANJ"));
print_endline (string_of_bool (double_metaphone "Annabel" = "ANPL"));
print_endline (string_of_bool (double_metaphone "Anne" = "AN"));
print_endline (string_of_bool (double_metaphone "Anstye" = "ANST"));
print_endline (string_of_bool (double_metaphone "Appling" = "APLN"));
print_endline (string_of_bool (double_metaphone "Apuke" = "APK"));
print_endline (string_of_bool (double_metaphone "Arnold" = "ARNL"));
print_endline (string_of_bool (double_metaphone "Ashby" = "AXP"));
print_endline (string_of_bool (double_metaphone "Astwood" = "ASTT"));
print_endline (string_of_bool (double_metaphone "Atkinson" = "ATKN"));
print_endline (string_of_bool (double_metaphone "Audley" = "ATL"));
print_endline (string_of_bool (double_metaphone "Austin" = "ASTN"));
print_endline (string_of_bool (double_metaphone "Avenal" = "AFNL"));
print_endline (string_of_bool (double_metaphone "Ayer" = "AR"));
print_endline (string_of_bool (double_metaphone "Ayot" = "AT"));
print_endline (string_of_bool (double_metaphone "Babbitt" = "PPT"));
print_endline (string_of_bool (double_metaphone "Bachelor" = "PXLR"));
print_endline (string_of_bool (double_metaphone "Bachelour" = "PXLR"));
print_endline (string_of_bool (double_metaphone "Bailey" = "PL"));
print_endline (string_of_bool (double_metaphone "Baivel" = "PFL"));
print_endline (string_of_bool (double_metaphone "Baker" = "PKR"));
print_endline (string_of_bool (double_metaphone "Baldwin" = "PLTN"));
print_endline (string_of_bool (double_metaphone "Balsley" = "PLSL"));
print_endline (string_of_bool (double_metaphone "Barber" = "PRPR"));
print_endline (string_of_bool (double_metaphone "Barker" = "PRKR"));
print_endline (string_of_bool (double_metaphone "Barlow" = "PRL"));
print_endline (string_of_bool (double_metaphone "Barnard" = "PRNR"));
print_endline (string_of_bool (double_metaphone "Barnes" = "PRNS"));
print_endline (string_of_bool (double_metaphone "Barnsley" = "PRNS"));
print_endline (string_of_bool (double_metaphone "Barouxis" = "PRKS"));
print_endline (string_of_bool (double_metaphone "Bartlet" = "PRTL"));
print_endline (string_of_bool (double_metaphone "Basley" = "PSL"));
print_endline (string_of_bool (double_metaphone "Basset" = "PST"));
print_endline (string_of_bool (double_metaphone "Bassett" = "PST"));
print_endline (string_of_bool (double_metaphone "Batchlor" = "PXLR"));
print_endline (string_of_bool (double_metaphone "Bates" = "PTS"));
print_endline (string_of_bool (double_metaphone "Batson" = "PTSN"));
print_endline (string_of_bool (double_metaphone "Bayes" = "PS"));
print_endline (string_of_bool (double_metaphone "Bayley" = "PL"));
print_endline (string_of_bool (double_metaphone "Beale" = "PL"));
print_endline (string_of_bool (double_metaphone "Beauchamp" = "PXMP"));
print_endline (string_of_bool (double_metaphone "Beauclerc" = "PKLR"));
print_endline (string_of_bool (double_metaphone "Beech" = "PK"));
print_endline (string_of_bool (double_metaphone "Beers" = "PRS"));
print_endline (string_of_bool (double_metaphone "Beke" = "PK"));
print_endline (string_of_bool (double_metaphone "Belcher" = "PLXR"));
print_endline (string_of_bool (double_metaphone "Benjamin" = "PNJM"));
print_endline (string_of_bool (double_metaphone "Benningham" = "PNNK"));
print_endline (string_of_bool (double_metaphone "Bereford" = "PRFR"));
print_endline (string_of_bool (double_metaphone "Bergen" = "PRJN"));
print_endline (string_of_bool (double_metaphone "Berkeley" = "PRKL"));
print_endline (string_of_bool (double_metaphone "Berry" = "PR"));
print_endline (string_of_bool (double_metaphone "Besse" = "PS"));
print_endline (string_of_bool (double_metaphone "Bessey" = "PS"));
print_endline (string_of_bool (double_metaphone "Bessiles" = "PSLS"));
print_endline (string_of_bool (double_metaphone "Bigelow" = "PJL")); (* !!!! *)
print_endline (string_of_bool (double_metaphone "Bigg" = "PK"));
print_endline (string_of_bool (double_metaphone "Bigod" = "PKT"));
print_endline (string_of_bool (double_metaphone "Billings" = "PLNK"));
print_endline (string_of_bool (double_metaphone "Bimper" = "PMPR"));
print_endline (string_of_bool (double_metaphone "Binker" = "PNKR"));
print_endline (string_of_bool (double_metaphone "Birdsill" = "PRTS"));
print_endline (string_of_bool (double_metaphone "Bishop" = "PXP"));
print_endline (string_of_bool (double_metaphone "Black" = "PLK"));
print_endline (string_of_bool (double_metaphone "Blagge" = "PLK"));
print_endline (string_of_bool (double_metaphone "Blake" = "PLK"));
print_endline (string_of_bool (double_metaphone "Blanck" = "PLNK"));
print_endline (string_of_bool (double_metaphone "Bledsoe" = "PLTS"));
print_endline (string_of_bool (double_metaphone "Blennerhasset" = "PLNR"));
print_endline (string_of_bool (double_metaphone "Blessing" = "PLSN"));
print_endline (string_of_bool (double_metaphone "Blewett" = "PLT"));
print_endline (string_of_bool (double_metaphone "Bloctgoed" = "PLKT"));
print_endline (string_of_bool (double_metaphone "Bloetgoet" = "PLTK"));
print_endline (string_of_bool (double_metaphone "Bloodgood" = "PLTK"));
print_endline (string_of_bool (double_metaphone "Blossom" = "PLSM"));
print_endline (string_of_bool (double_metaphone "Blount" = "PLNT"));
print_endline (string_of_bool (double_metaphone "Bodine" = "PTN"));
print_endline (string_of_bool (double_metaphone "Bodman" = "PTMN"));
print_endline (string_of_bool (double_metaphone "BonCoeur" = "PNKR"));
print_endline (string_of_bool (double_metaphone "Bond" = "PNT"));
print_endline (string_of_bool (double_metaphone "Boscawen" = "PSKN"));
print_endline (string_of_bool (double_metaphone "Bosworth" = "PSR0"));
print_endline (string_of_bool (double_metaphone "Bouchier" = "PX"));
print_endline (string_of_bool (double_metaphone "Bowne" = "PN"));
print_endline (string_of_bool (double_metaphone "Bradbury" = "PRTP"));
print_endline (string_of_bool (double_metaphone "Bradder" = "PRTR"));
print_endline (string_of_bool (double_metaphone "Bradford" = "PRTF"));
print_endline (string_of_bool (double_metaphone "Bradstreet" = "PRTS"));
print_endline (string_of_bool (double_metaphone "Braham" = "PRHM"));
print_endline (string_of_bool (double_metaphone "Brailsford" = "PRLS"));
print_endline (string_of_bool (double_metaphone "Brainard" = "PRNR"));
print_endline (string_of_bool (double_metaphone "Brandish" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Braun" = "PRN"));
print_endline (string_of_bool (double_metaphone "Brecc" = "PRK"));
print_endline (string_of_bool (double_metaphone "Brent" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Brenton" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Briggs" = "PRKS"));
print_endline (string_of_bool (double_metaphone "Brigham" = "PRM"));
print_endline (string_of_bool (double_metaphone "Brobst" = "PRPS"));
print_endline (string_of_bool (double_metaphone "Brome" = "PRM"));
print_endline (string_of_bool (double_metaphone "Bronson" = "PRNS"));
print_endline (string_of_bool (double_metaphone "Brooks" = "PRKS"));
print_endline (string_of_bool (double_metaphone "Brouillard" = "PRLR"));
print_endline (string_of_bool (double_metaphone "Brown" = "PRN"));
print_endline (string_of_bool (double_metaphone "Browne" = "PRN"));
print_endline (string_of_bool (double_metaphone "Brownell" = "PRNL"));
print_endline (string_of_bool (double_metaphone "Bruley" = "PRL"));
print_endline (string_of_bool (double_metaphone "Bryant" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Brzozowski" = "PRSS"));
print_endline (string_of_bool (double_metaphone "Buide" = "PT"));
print_endline (string_of_bool (double_metaphone "Bulmer" = "PLMR"));
print_endline (string_of_bool (double_metaphone "Bunker" = "PNKR"));
print_endline (string_of_bool (double_metaphone "Burden" = "PRTN"));
print_endline (string_of_bool (double_metaphone "Burge" = "PRJ"));
print_endline (string_of_bool (double_metaphone "Burgoyne" = "PRKN"));
print_endline (string_of_bool (double_metaphone "Burke" = "PRK"));
print_endline (string_of_bool (double_metaphone "Burnett" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Burpee" = "PRP"));
print_endline (string_of_bool (double_metaphone "Bursley" = "PRSL"));
print_endline (string_of_bool (double_metaphone "Burton" = "PRTN"));
print_endline (string_of_bool (double_metaphone "Bushnell" = "PXNL"));
print_endline (string_of_bool (double_metaphone "Buss" = "PS"));
print_endline (string_of_bool (double_metaphone "Buswell" = "PSL"));
print_endline (string_of_bool (double_metaphone "Butler" = "PTLR"));
print_endline (string_of_bool (double_metaphone "Calkin" = "KLKN"));
print_endline (string_of_bool (double_metaphone "Canada" = "KNT"));
print_endline (string_of_bool (double_metaphone "Canmore" = "KNMR"));
print_endline (string_of_bool (double_metaphone "Canney" = "KN"));
print_endline (string_of_bool (double_metaphone "Capet" = "KPT"));
print_endline (string_of_bool (double_metaphone "Card" = "KRT"));
print_endline (string_of_bool (double_metaphone "Carman" = "KRMN"));
print_endline (string_of_bool (double_metaphone "Carpenter" = "KRPN"));
print_endline (string_of_bool (double_metaphone "Cartwright" = "KRTR"));
print_endline (string_of_bool (double_metaphone "Casey" = "KS"));
print_endline (string_of_bool (double_metaphone "Catterfield" = "KTRF"));
print_endline (string_of_bool (double_metaphone "Ceeley" = "SL"));
print_endline (string_of_bool (double_metaphone "Chambers" = "XMPR"));
print_endline (string_of_bool (double_metaphone "Champion" = "XMPN"));
print_endline (string_of_bool (double_metaphone "Chapman" = "XPMN"));
print_endline (string_of_bool (double_metaphone "Chase" = "XS"));
print_endline (string_of_bool (double_metaphone "Cheney" = "XN"));
print_endline (string_of_bool (double_metaphone "Chetwynd" = "XTNT"));
print_endline (string_of_bool (double_metaphone "Chevalier" = "XFL"));
print_endline (string_of_bool (double_metaphone "Chillingsworth" = "XLNK"));
print_endline (string_of_bool (double_metaphone "Christie" = "KRST"));
print_endline (string_of_bool (double_metaphone "Chubbuck" = "XPK"));
print_endline (string_of_bool (double_metaphone "Church" = "XRX"));
print_endline (string_of_bool (double_metaphone "Clark" = "KLRK"));
print_endline (string_of_bool (double_metaphone "Clarke" = "KLRK"));
print_endline (string_of_bool (double_metaphone "Cleare" = "KLR"));
print_endline (string_of_bool (double_metaphone "Clement" = "KLMN"));
print_endline (string_of_bool (double_metaphone "Clerke" = "KLRK"));
print_endline (string_of_bool (double_metaphone "Clibben" = "KLPN"));
print_endline (string_of_bool (double_metaphone "Clifford" = "KLFR"));
print_endline (string_of_bool (double_metaphone "Clivedon" = "KLFT"));
print_endline (string_of_bool (double_metaphone "Close" = "KLS"));
print_endline (string_of_bool (double_metaphone "Clothilde" = "KL0L"));
print_endline (string_of_bool (double_metaphone "Cobb" = "KP"));
print_endline (string_of_bool (double_metaphone "Coburn" = "KPRN"));
print_endline (string_of_bool (double_metaphone "Coburne" = "KPRN"));
print_endline (string_of_bool (double_metaphone "Cocke" = "KK"));
print_endline (string_of_bool (double_metaphone "Coffin" = "KFN"));
print_endline (string_of_bool (double_metaphone "Coffyn" = "KFN"));
print_endline (string_of_bool (double_metaphone "Colborne" = "KLPR"));
print_endline (string_of_bool (double_metaphone "Colby" = "KLP"));
print_endline (string_of_bool (double_metaphone "Cole" = "KL"));
print_endline (string_of_bool (double_metaphone "Coleman" = "KLMN"));
print_endline (string_of_bool (double_metaphone "Collier" = "KL"));
print_endline (string_of_bool (double_metaphone "Compton" = "KMPT"));
print_endline (string_of_bool (double_metaphone "Cone" = "KN"));
print_endline (string_of_bool (double_metaphone "Cook" = "KK"));
print_endline (string_of_bool (double_metaphone "Cooke" = "KK"));
print_endline (string_of_bool (double_metaphone "Cooper" = "KPR"));
print_endline (string_of_bool (double_metaphone "Copperthwaite" = "KPR0"));
print_endline (string_of_bool (double_metaphone "Corbet" = "KRPT"));
print_endline (string_of_bool (double_metaphone "Corell" = "KRL"));
print_endline (string_of_bool (double_metaphone "Corey" = "KR"));
print_endline (string_of_bool (double_metaphone "Corlies" = "KRLS"));
print_endline (string_of_bool (double_metaphone "Corneliszen" = "KRNL"));
print_endline (string_of_bool (double_metaphone "Cornelius" = "KRNL"));
print_endline (string_of_bool (double_metaphone "Cornwallis" = "KRNL"));
print_endline (string_of_bool (double_metaphone "Cosgrove" = "KSKR"));
print_endline (string_of_bool (double_metaphone "Count of Brionne" = "KNTF"));
print_endline (string_of_bool (double_metaphone "Covill" = "KFL"));
print_endline (string_of_bool (double_metaphone "Cowperthwaite" = "KPR0"));
print_endline (string_of_bool (double_metaphone "Cowperwaite" = "KPRT"));
print_endline (string_of_bool (double_metaphone "Crane" = "KRN"));
print_endline (string_of_bool (double_metaphone "Creagmile" = "KRKM"));
print_endline (string_of_bool (double_metaphone "Crew" = "KR"));
print_endline (string_of_bool (double_metaphone "Crispin" = "KRSP"));
print_endline (string_of_bool (double_metaphone "Crocker" = "KRKR"));
print_endline (string_of_bool (double_metaphone "Crockett" = "KRKT"));
print_endline (string_of_bool (double_metaphone "Crosby" = "KRSP"));
print_endline (string_of_bool (double_metaphone "Crump" = "KRMP"));
print_endline (string_of_bool (double_metaphone "Cunningham" = "KNNK"));
print_endline (string_of_bool (double_metaphone "Curtis" = "KRTS"));
print_endline (string_of_bool (double_metaphone "Cutha" = "K0"));
print_endline (string_of_bool (double_metaphone "Cutter" = "KTR"));
print_endline (string_of_bool (double_metaphone "D'Aubigny" = "TPN"));
print_endline (string_of_bool (double_metaphone "DAVIS" = "TFS"));
print_endline (string_of_bool (double_metaphone "Dabinott" = "TPNT"));
print_endline (string_of_bool (double_metaphone "Dacre" = "TKR"));
print_endline (string_of_bool (double_metaphone "Daggett" = "TKT"));
print_endline (string_of_bool (double_metaphone "Danvers" = "TNFR"));
print_endline (string_of_bool (double_metaphone "Darcy" = "TRS"));
print_endline (string_of_bool (double_metaphone "Davis" = "TFS"));
print_endline (string_of_bool (double_metaphone "Dawn" = "TN"));
print_endline (string_of_bool (double_metaphone "Dawson" = "TSN"));
print_endline (string_of_bool (double_metaphone "Day" = "T"));
print_endline (string_of_bool (double_metaphone "Daye" = "T"));
print_endline (string_of_bool (double_metaphone "DeGrenier" = "TKRN"));
print_endline (string_of_bool (double_metaphone "Dean" = "TN"));
print_endline (string_of_bool (double_metaphone "Deekindaugh" = "TKNT"));
print_endline (string_of_bool (double_metaphone "Dennis" = "TNS"));
print_endline (string_of_bool (double_metaphone "Denny" = "TN"));
print_endline (string_of_bool (double_metaphone "Denton" = "TNTN"));
print_endline (string_of_bool (double_metaphone "Desborough" = "TSPR"));
print_endline (string_of_bool (double_metaphone "Despenser" = "TSPN"));
print_endline (string_of_bool (double_metaphone "Deverill" = "TFRL"));
print_endline (string_of_bool (double_metaphone "Devine" = "TFN"));
print_endline (string_of_bool (double_metaphone "Dexter" = "TKST"));
print_endline (string_of_bool (double_metaphone "Dillaway" = "TL"));
print_endline (string_of_bool (double_metaphone "Dimmick" = "TMK"));
print_endline (string_of_bool (double_metaphone "Dinan" = "TNN"));
print_endline (string_of_bool (double_metaphone "Dix" = "TKS"));
print_endline (string_of_bool (double_metaphone "Doggett" = "TKT"));
print_endline (string_of_bool (double_metaphone "Donahue" = "TNH"));
print_endline (string_of_bool (double_metaphone "Dorfman" = "TRFM"));
print_endline (string_of_bool (double_metaphone "Dorris" = "TRS"));
print_endline (string_of_bool (double_metaphone "Dow" = "T"));
print_endline (string_of_bool (double_metaphone "Downey" = "TN"));
print_endline (string_of_bool (double_metaphone "Downing" = "TNNK"));
print_endline (string_of_bool (double_metaphone "Dowsett" = "TST"));
print_endline (string_of_bool (double_metaphone "Duck?" = "TK"));
print_endline (string_of_bool (double_metaphone "Dudley" = "TTL"));
print_endline (string_of_bool (double_metaphone "Duffy" = "TF"));
print_endline (string_of_bool (double_metaphone "Dunn" = "TN"));
print_endline (string_of_bool (double_metaphone "Dunsterville" = "TNST"));
print_endline (string_of_bool (double_metaphone "Durrant" = "TRNT"));
print_endline (string_of_bool (double_metaphone "Durrin" = "TRN"));
print_endline (string_of_bool (double_metaphone "Dustin" = "TSTN"));
print_endline (string_of_bool (double_metaphone "Duston" = "TSTN"));
print_endline (string_of_bool (double_metaphone "Eames" = "AMS"));
print_endline (string_of_bool (double_metaphone "Early" = "ARL"));
print_endline (string_of_bool (double_metaphone "Easty" = "AST"));
print_endline (string_of_bool (double_metaphone "Ebbett" = "APT"));
print_endline (string_of_bool (double_metaphone "Eberbach" = "APRP"));
print_endline (string_of_bool (double_metaphone "Eberhard" = "APRR"));
print_endline (string_of_bool (double_metaphone "Eddy" = "AT"));
print_endline (string_of_bool (double_metaphone "Edenden" = "ATNT"));
print_endline (string_of_bool (double_metaphone "Edwards" = "ATRT"));
print_endline (string_of_bool (double_metaphone "Eglinton" = "AKLN"));
print_endline (string_of_bool (double_metaphone "Eliot" = "ALT"));
print_endline (string_of_bool (double_metaphone "Elizabeth" = "ALSP"));
print_endline (string_of_bool (double_metaphone "Ellis" = "ALS"));
print_endline (string_of_bool (double_metaphone "Ellison" = "ALSN"));
print_endline (string_of_bool (double_metaphone "Ellot" = "ALT"));
print_endline (string_of_bool (double_metaphone "Elny" = "ALN"));
print_endline (string_of_bool (double_metaphone "Elsner" = "ALSN"));
print_endline (string_of_bool (double_metaphone "Emerson" = "AMRS"));
print_endline (string_of_bool (double_metaphone "Empson" = "AMPS"));
print_endline (string_of_bool (double_metaphone "Est" = "AST"));
print_endline (string_of_bool (double_metaphone "Estabrook" = "ASTP"));
print_endline (string_of_bool (double_metaphone "Estes" = "ASTS"));
print_endline (string_of_bool (double_metaphone "Estey" = "AST"));
print_endline (string_of_bool (double_metaphone "Evans" = "AFNS"));
print_endline (string_of_bool (double_metaphone "Fallowell" = "FLL"));
print_endline (string_of_bool (double_metaphone "Farnsworth" = "FRNS"));
print_endline (string_of_bool (double_metaphone "Feake" = "FK"));
print_endline (string_of_bool (double_metaphone "Feke" = "FK"));
print_endline (string_of_bool (double_metaphone "Fellows" = "FLS"));
print_endline (string_of_bool (double_metaphone "Fettiplace" = "FTPL"));
print_endline (string_of_bool (double_metaphone "Finney" = "FN"));
print_endline (string_of_bool (double_metaphone "Fischer" = "FXR"));
print_endline (string_of_bool (double_metaphone "Fisher" = "FXR"));
print_endline (string_of_bool (double_metaphone "Fisk" = "FSK"));
print_endline (string_of_bool (double_metaphone "Fiske" = "FSK"));
print_endline (string_of_bool (double_metaphone "Fletcher" = "FLXR"));
print_endline (string_of_bool (double_metaphone "Folger" = "FLKR"));
print_endline (string_of_bool (double_metaphone "Foliot" = "FLT"));
print_endline (string_of_bool (double_metaphone "Folyot" = "FLT"));
print_endline (string_of_bool (double_metaphone "Fones" = "FNS"));
print_endline (string_of_bool (double_metaphone "Fordham" = "FRTM"));
print_endline (string_of_bool (double_metaphone "Forstner" = "FRST"));
print_endline (string_of_bool (double_metaphone "Fosten" = "FSTN"));
print_endline (string_of_bool (double_metaphone "Foster" = "FSTR"));
print_endline (string_of_bool (double_metaphone "Foulke" = "FLK"));
print_endline (string_of_bool (double_metaphone "Fowler" = "FLR"));
print_endline (string_of_bool (double_metaphone "Foxwell" = "FKSL"));
print_endline (string_of_bool (double_metaphone "Fraley" = "FRL"));
print_endline (string_of_bool (double_metaphone "Franceys" = "FRNS"));
print_endline (string_of_bool (double_metaphone "Franke" = "FRNK"));
print_endline (string_of_bool (double_metaphone "Frascella" = "FRSL")); (* !!! *)
print_endline (string_of_bool (double_metaphone "Frazer" = "FRSR"));
print_endline (string_of_bool (double_metaphone "Fredd" = "FRT"));
print_endline (string_of_bool (double_metaphone "Freeman" = "FRMN"));
print_endline (string_of_bool (double_metaphone "French" = "FRNX"));
print_endline (string_of_bool (double_metaphone "Freville" = "FRFL"));
print_endline (string_of_bool (double_metaphone "Frey" = "FR"));
print_endline (string_of_bool (double_metaphone "Frick" = "FRK"));
print_endline (string_of_bool (double_metaphone "Frier" = "FR"));
print_endline (string_of_bool (double_metaphone "Froe" = "FR"));
print_endline (string_of_bool (double_metaphone "Frorer" = "FRRR"));
print_endline (string_of_bool (double_metaphone "Frost" = "FRST"));
print_endline (string_of_bool (double_metaphone "Frothingham" = "FR0N"));
print_endline (string_of_bool (double_metaphone "Fry" = "FR"));
print_endline (string_of_bool (double_metaphone "Gaffney" = "KFN"));
print_endline (string_of_bool (double_metaphone "Gage" = "KJ"));
print_endline (string_of_bool (double_metaphone "Gallion" = "KLN"));
print_endline (string_of_bool (double_metaphone "Gallishan" = "KLXN"));
print_endline (string_of_bool (double_metaphone "Gamble" = "KMPL"));
print_endline (string_of_bool (double_metaphone "Garbrand" = "KRPR"));
print_endline (string_of_bool (double_metaphone "Gardner" = "KRTN"));
print_endline (string_of_bool (double_metaphone "Garrett" = "KRT"));
print_endline (string_of_bool (double_metaphone "Gassner" = "KSNR"));
print_endline (string_of_bool (double_metaphone "Gater" = "KTR"));
print_endline (string_of_bool (double_metaphone "Gaunt" = "KNT"));
print_endline (string_of_bool (double_metaphone "Gayer" = "KR"));
print_endline (string_of_bool (double_metaphone "Gerken" = "KRKN"));
print_endline (string_of_bool (double_metaphone "Gerritsen" = "KRTS"));
print_endline (string_of_bool (double_metaphone "Gibbs" = "KPS"));
print_endline (string_of_bool (double_metaphone "Giffard" = "JFRT"));
print_endline (string_of_bool (double_metaphone "Gilbert" = "KLPR"));
print_endline (string_of_bool (double_metaphone "Gill" = "KL"));
print_endline (string_of_bool (double_metaphone "Gilman" = "KLMN"));
print_endline (string_of_bool (double_metaphone "Glass" = "KLS"));
(*print_endline (string_of_bool (double_metaphone "Goddard\Gifford" = "KTRT"));*)
print_endline (string_of_bool (double_metaphone "Godfrey" = "KTFR"));
print_endline (string_of_bool (double_metaphone "Godwin" = "KTN"));
print_endline (string_of_bool (double_metaphone "Goodale" = "KTL"));
print_endline (string_of_bool (double_metaphone "Goodnow" = "KTN"));
print_endline (string_of_bool (double_metaphone "Gorham" = "KRM"));
print_endline (string_of_bool (double_metaphone "Goseline" = "KSLN"));
print_endline (string_of_bool (double_metaphone "Gott" = "KT"));
print_endline (string_of_bool (double_metaphone "Gould" = "KLT"));
print_endline (string_of_bool (double_metaphone "Grafton" = "KRFT"));
print_endline (string_of_bool (double_metaphone "Grant" = "KRNT"));
print_endline (string_of_bool (double_metaphone "Gray" = "KR"));
print_endline (string_of_bool (double_metaphone "Green" = "KRN"));
print_endline (string_of_bool (double_metaphone "Griffin" = "KRFN"));
print_endline (string_of_bool (double_metaphone "Grill" = "KRL"));
print_endline (string_of_bool (double_metaphone "Grim" = "KRM"));
print_endline (string_of_bool (double_metaphone "Grisgonelle" = "KRSK"));
print_endline (string_of_bool (double_metaphone "Gross" = "KRS"));
print_endline (string_of_bool (double_metaphone "Guba" = "KP"));
print_endline (string_of_bool (double_metaphone "Gybbes" = "KPS"));
print_endline (string_of_bool (double_metaphone "Haburne" = "HPRN"));
print_endline (string_of_bool (double_metaphone "Hackburne" = "HKPR"));
print_endline (string_of_bool (double_metaphone "Haddon?" = "HTN"));
print_endline (string_of_bool (double_metaphone "Haines" = "HNS"));
print_endline (string_of_bool (double_metaphone "Hale" = "HL"));
print_endline (string_of_bool (double_metaphone "Hall" = "HL"));
print_endline (string_of_bool (double_metaphone "Hallet" = "HLT"));
print_endline (string_of_bool (double_metaphone "Hallock" = "HLK"));
print_endline (string_of_bool (double_metaphone "Halstead" = "HLST"));
print_endline (string_of_bool (double_metaphone "Hammond" = "HMNT"));
print_endline (string_of_bool (double_metaphone "Hance" = "HNS"));
print_endline (string_of_bool (double_metaphone "Handy" = "HNT"));
print_endline (string_of_bool (double_metaphone "Hanson" = "HNSN"));
print_endline (string_of_bool (double_metaphone "Harasek" = "HRSK"));
print_endline (string_of_bool (double_metaphone "Harcourt" = "HRKR"));
print_endline (string_of_bool (double_metaphone "Hardy" = "HRT"));
print_endline (string_of_bool (double_metaphone "Harlock" = "HRLK"));
print_endline (string_of_bool (double_metaphone "Harris" = "HRS"));
print_endline (string_of_bool (double_metaphone "Hartley" = "HRTL"));
print_endline (string_of_bool (double_metaphone "Harvey" = "HRF"));
print_endline (string_of_bool (double_metaphone "Harvie" = "HRF"));
print_endline (string_of_bool (double_metaphone "Harwood" = "HRT"));
print_endline (string_of_bool (double_metaphone "Hathaway" = "H0"));
print_endline (string_of_bool (double_metaphone "Haukeness" = "HKNS"));
print_endline (string_of_bool (double_metaphone "Hawkes" = "HKS"));
print_endline (string_of_bool (double_metaphone "Hawkhurst" = "HKRS"));
print_endline (string_of_bool (double_metaphone "Hawkins" = "HKNS"));
print_endline (string_of_bool (double_metaphone "Hawley" = "HL"));
print_endline (string_of_bool (double_metaphone "Heald" = "HLT"));
print_endline (string_of_bool (double_metaphone "Helsdon" = "HLST"));
print_endline (string_of_bool (double_metaphone "Hemenway" = "HMN"));
print_endline (string_of_bool (double_metaphone "Hemmenway" = "HMN"));
print_endline (string_of_bool (double_metaphone "Henck" = "HNK"));
print_endline (string_of_bool (double_metaphone "Henderson" = "HNTR"));
print_endline (string_of_bool (double_metaphone "Hendricks" = "HNTR"));
print_endline (string_of_bool (double_metaphone "Hersey" = "HRS"));
print_endline (string_of_bool (double_metaphone "Hewes" = "HS"));
print_endline (string_of_bool (double_metaphone "Heyman" = "HMN"));
print_endline (string_of_bool (double_metaphone "Hicks" = "HKS"));
print_endline (string_of_bool (double_metaphone "Hidden" = "HTN"));
print_endline (string_of_bool (double_metaphone "Higgs" = "HKS"));
print_endline (string_of_bool (double_metaphone "Hill" = "HL"));
print_endline (string_of_bool (double_metaphone "Hills" = "HLS"));
print_endline (string_of_bool (double_metaphone "Hinckley" = "HNKL"));
print_endline (string_of_bool (double_metaphone "Hipwell" = "HPL"));
print_endline (string_of_bool (double_metaphone "Hobart" = "HPRT"));
print_endline (string_of_bool (double_metaphone "Hoben" = "HPN"));
print_endline (string_of_bool (double_metaphone "Hoffmann" = "HFMN"));
print_endline (string_of_bool (double_metaphone "Hogan" = "HKN"));
print_endline (string_of_bool (double_metaphone "Holmes" = "HLMS"));
print_endline (string_of_bool (double_metaphone "Hoo" = "H"));
print_endline (string_of_bool (double_metaphone "Hooker" = "HKR"));
print_endline (string_of_bool (double_metaphone "Hopcott" = "HPKT"));
print_endline (string_of_bool (double_metaphone "Hopkins" = "HPKN"));
print_endline (string_of_bool (double_metaphone "Hopkinson" = "HPKN"));
print_endline (string_of_bool (double_metaphone "Hornsey" = "HRNS"));
print_endline (string_of_bool (double_metaphone "Houckgeest" = "HKJS"));
print_endline (string_of_bool (double_metaphone "Hough" = "H"));
print_endline (string_of_bool (double_metaphone "Houstin" = "HSTN"));
print_endline (string_of_bool (double_metaphone "How" = "H"));
print_endline (string_of_bool (double_metaphone "Howe" = "H"));
print_endline (string_of_bool (double_metaphone "Howland" = "HLNT"));
print_endline (string_of_bool (double_metaphone "Hubner" = "HPNR"));
print_endline (string_of_bool (double_metaphone "Hudnut" = "HTNT"));
print_endline (string_of_bool (double_metaphone "Hughes" = "HS"));
print_endline (string_of_bool (double_metaphone "Hull" = "HL"));
print_endline (string_of_bool (double_metaphone "Hulme" = "HLM"));
print_endline (string_of_bool (double_metaphone "Hume" = "HM"));
print_endline (string_of_bool (double_metaphone "Hundertumark" = "HNTR"));
print_endline (string_of_bool (double_metaphone "Hundley" = "HNTL"));
print_endline (string_of_bool (double_metaphone "Hungerford" = "HNKR"));
print_endline (string_of_bool (double_metaphone "Hunt" = "HNT"));
print_endline (string_of_bool (double_metaphone "Hurst" = "HRST"));
print_endline (string_of_bool (double_metaphone "Husbands" = "HSPN"));
print_endline (string_of_bool (double_metaphone "Hussey" = "HS"));
print_endline (string_of_bool (double_metaphone "Husted" = "HSTT"));
print_endline (string_of_bool (double_metaphone "Hutchins" = "HXNS"));
print_endline (string_of_bool (double_metaphone "Hutchinson" = "HXNS"));
print_endline (string_of_bool (double_metaphone "Huttinger" = "HTNK"));
print_endline (string_of_bool (double_metaphone "Huybertsen" = "HPRT"));
print_endline (string_of_bool (double_metaphone "Iddenden" = "ATNT"));
print_endline (string_of_bool (double_metaphone "Ingraham" = "ANKR"));
print_endline (string_of_bool (double_metaphone "Ives" = "AFS"));
print_endline (string_of_bool (double_metaphone "Jackson" = "JKSN"));
print_endline (string_of_bool (double_metaphone "Jacob" = "JKP"));
print_endline (string_of_bool (double_metaphone "Jans" = "JNS"));
print_endline (string_of_bool (double_metaphone "Jenkins" = "JNKN"));
print_endline (string_of_bool (double_metaphone "Jewett" = "JT"));
print_endline (string_of_bool (double_metaphone "Jewitt" = "JT"));
print_endline (string_of_bool (double_metaphone "Johnson" = "JNSN"));
print_endline (string_of_bool (double_metaphone "Jones" = "JNS"));
print_endline (string_of_bool (double_metaphone "Josephine" = "JSFN"));
print_endline (string_of_bool (double_metaphone "Judd" = "JT"));
print_endline (string_of_bool (double_metaphone "June" = "JN"));
print_endline (string_of_bool (double_metaphone "Kamarowska" = "KMRS"));
print_endline (string_of_bool (double_metaphone "Kay" = "K"));
print_endline (string_of_bool (double_metaphone "Kelley" = "KL"));
print_endline (string_of_bool (double_metaphone "Kelly" = "KL"));
print_endline (string_of_bool (double_metaphone "Keymber" = "KMPR"));
print_endline (string_of_bool (double_metaphone "Keynes" = "KNS"));
print_endline (string_of_bool (double_metaphone "Kilham" = "KLM"));
print_endline (string_of_bool (double_metaphone "Kim" = "KM"));
print_endline (string_of_bool (double_metaphone "Kimball" = "KMPL"));
print_endline (string_of_bool (double_metaphone "King" = "KNK"));
print_endline (string_of_bool (double_metaphone "Kinsey" = "KNS"));
print_endline (string_of_bool (double_metaphone "Kirk" = "KRK"));
print_endline (string_of_bool (double_metaphone "Kirton" = "KRTN"));
print_endline (string_of_bool (double_metaphone "Kistler" = "KSTL"));
print_endline (string_of_bool (double_metaphone "Kitchen" = "KXN"));
print_endline (string_of_bool (double_metaphone "Kitson" = "KTSN"));
print_endline (string_of_bool (double_metaphone "Klett" = "KLT"));
print_endline (string_of_bool (double_metaphone "Kline" = "KLN"));
print_endline (string_of_bool (double_metaphone "Knapp" = "NP"));
print_endline (string_of_bool (double_metaphone "Knight" = "NT"));
print_endline (string_of_bool (double_metaphone "Knote" = "NT"));
print_endline (string_of_bool (double_metaphone "Knott" = "NT"));
print_endline (string_of_bool (double_metaphone "Knox" = "NKS"));
print_endline (string_of_bool (double_metaphone "Koeller" = "KLR"));
print_endline (string_of_bool (double_metaphone "La Pointe" = "LPNT"));
print_endline (string_of_bool (double_metaphone "LaPlante" = "LPLN"));
print_endline (string_of_bool (double_metaphone "Laimbeer" = "LMPR"));
print_endline (string_of_bool (double_metaphone "Lamb" = "LMP"));
print_endline (string_of_bool (double_metaphone "Lambertson" = "LMPR"));
print_endline (string_of_bool (double_metaphone "Lancto" = "LNKT"));
print_endline (string_of_bool (double_metaphone "Landry" = "LNTR"));
print_endline (string_of_bool (double_metaphone "Lane" = "LN"));
print_endline (string_of_bool (double_metaphone "Langendyck" = "LNJN"));
print_endline (string_of_bool (double_metaphone "Langer" = "LNKR"));
print_endline (string_of_bool (double_metaphone "Langford" = "LNKF"));
print_endline (string_of_bool (double_metaphone "Lantersee" = "LNTR"));
print_endline (string_of_bool (double_metaphone "Laquer" = "LKR"));
print_endline (string_of_bool (double_metaphone "Larkin" = "LRKN"));
print_endline (string_of_bool (double_metaphone "Latham" = "LTM"));
print_endline (string_of_bool (double_metaphone "Lathrop" = "L0RP"));
print_endline (string_of_bool (double_metaphone "Lauter" = "LTR"));
print_endline (string_of_bool (double_metaphone "Lawrence" = "LRNS"));
print_endline (string_of_bool (double_metaphone "Leach" = "LK"));
print_endline (string_of_bool (double_metaphone "Leager" = "LKR"));
print_endline (string_of_bool (double_metaphone "Learned" = "LRNT"));
print_endline (string_of_bool (double_metaphone "Leavitt" = "LFT"));
print_endline (string_of_bool (double_metaphone "Lee" = "L"));
print_endline (string_of_bool (double_metaphone "Leete" = "LT"));
print_endline (string_of_bool (double_metaphone "Leggett" = "LKT"));
print_endline (string_of_bool (double_metaphone "Leland" = "LLNT"));
print_endline (string_of_bool (double_metaphone "Leonard" = "LNRT"));
print_endline (string_of_bool (double_metaphone "Lester" = "LSTR"));
print_endline (string_of_bool (double_metaphone "Lestrange" = "LSTR"));
print_endline (string_of_bool (double_metaphone "Lethem" = "L0M"));
print_endline (string_of_bool (double_metaphone "Levine" = "LFN"));
print_endline (string_of_bool (double_metaphone "Lewes" = "LS"));
print_endline (string_of_bool (double_metaphone "Lewis" = "LS"));
print_endline (string_of_bool (double_metaphone "Lincoln" = "LNKL"));
print_endline (string_of_bool (double_metaphone "Lindsey" = "LNTS"));
print_endline (string_of_bool (double_metaphone "Linher" = "LNR"));
print_endline (string_of_bool (double_metaphone "Lippet" = "LPT"));
print_endline (string_of_bool (double_metaphone "Lippincott" = "LPNK"));
print_endline (string_of_bool (double_metaphone "Lockwood" = "LKT"));
print_endline (string_of_bool (double_metaphone "Loines" = "LNS"));
print_endline (string_of_bool (double_metaphone "Lombard" = "LMPR"));
print_endline (string_of_bool (double_metaphone "Long" = "LNK"));
print_endline (string_of_bool (double_metaphone "Longespee" = "LNJS"));
print_endline (string_of_bool (double_metaphone "Look" = "LK"));
print_endline (string_of_bool (double_metaphone "Lounsberry" = "LNSP"));
print_endline (string_of_bool (double_metaphone "Lounsbury" = "LNSP"));
print_endline (string_of_bool (double_metaphone "Louthe" = "L0"));
print_endline (string_of_bool (double_metaphone "Loveyne" = "LFN"));
print_endline (string_of_bool (double_metaphone "Lowe" = "L"));
print_endline (string_of_bool (double_metaphone "Ludlam" = "LTLM"));
print_endline (string_of_bool (double_metaphone "Lumbard" = "LMPR"));
print_endline (string_of_bool (double_metaphone "Lund" = "LNT"));
print_endline (string_of_bool (double_metaphone "Luno" = "LN"));
print_endline (string_of_bool (double_metaphone "Lutz" = "LTS"));
print_endline (string_of_bool (double_metaphone "Lydia" = "LT"));
print_endline (string_of_bool (double_metaphone "Lynne" = "LN"));
print_endline (string_of_bool (double_metaphone "Lyon" = "LN"));
print_endline (string_of_bool (double_metaphone "MacAlpin" = "MKLP"));
print_endline (string_of_bool (double_metaphone "MacBricc" = "MKPR"));
print_endline (string_of_bool (double_metaphone "MacCrinan" = "MKRN"));
print_endline (string_of_bool (double_metaphone "MacKenneth" = "MKN0"));
print_endline (string_of_bool (double_metaphone "MacMael nam Bo" = "MKML"));
print_endline (string_of_bool (double_metaphone "MacMurchada" = "MKMR"));
print_endline (string_of_bool (double_metaphone "Macomber" = "MKMP"));
print_endline (string_of_bool (double_metaphone "Macy" = "MS"));
print_endline (string_of_bool (double_metaphone "Magnus" = "MNS"));
print_endline (string_of_bool (double_metaphone "Mahien" = "MHN"));
print_endline (string_of_bool (double_metaphone "Malmains" = "MLMN"));
print_endline (string_of_bool (double_metaphone "Malory" = "MLR"));
print_endline (string_of_bool (double_metaphone "Mancinelli" = "MNSN"));
print_endline (string_of_bool (double_metaphone "Mancini" = "MNSN"));
print_endline (string_of_bool (double_metaphone "Mann" = "MN"));
print_endline (string_of_bool (double_metaphone "Manning" = "MNNK"));
print_endline (string_of_bool (double_metaphone "Manter" = "MNTR"));
print_endline (string_of_bool (double_metaphone "Marion" = "MRN"));
print_endline (string_of_bool (double_metaphone "Marley" = "MRL"));
print_endline (string_of_bool (double_metaphone "Marmion" = "MRMN"));
print_endline (string_of_bool (double_metaphone "Marquart" = "MRKR"));
print_endline (string_of_bool (double_metaphone "Marsh" = "MRX"));
print_endline (string_of_bool (double_metaphone "Marshal" = "MRXL"));
print_endline (string_of_bool (double_metaphone "Marshall" = "MRXL"));
print_endline (string_of_bool (double_metaphone "Martel" = "MRTL"));
print_endline (string_of_bool (double_metaphone "Martha" = "MR0"));
print_endline (string_of_bool (double_metaphone "Martin" = "MRTN"));
print_endline (string_of_bool (double_metaphone "Marturano" = "MRTR"));
print_endline (string_of_bool (double_metaphone "Marvin" = "MRFN"));
print_endline (string_of_bool (double_metaphone "Mary" = "MR"));
print_endline (string_of_bool (double_metaphone "Mason" = "MSN"));
print_endline (string_of_bool (double_metaphone "Maxwell" = "MKSL"));
print_endline (string_of_bool (double_metaphone "Mayhew" = "MH"));
print_endline (string_of_bool (double_metaphone "McAllaster" = "MKLS"));
print_endline (string_of_bool (double_metaphone "McAllister" = "MKLS"));
print_endline (string_of_bool (double_metaphone "McConnell" = "MKNL"));
print_endline (string_of_bool (double_metaphone "McFarland" = "MKFR"));
print_endline (string_of_bool (double_metaphone "McIlroy" = "MSLR"));
print_endline (string_of_bool (double_metaphone "McNair" = "MKNR"));
print_endline (string_of_bool (double_metaphone "McNair-Landry" = "MKNR"));
print_endline (string_of_bool (double_metaphone "McRaven" = "MKRF"));
print_endline (string_of_bool (double_metaphone "Mead" = "MT"));
print_endline (string_of_bool (double_metaphone "Meade" = "MT"));
print_endline (string_of_bool (double_metaphone "Meck" = "MK"));
print_endline (string_of_bool (double_metaphone "Melton" = "MLTN"));
print_endline (string_of_bool (double_metaphone "Mendenhall" = "MNTN"));
print_endline (string_of_bool (double_metaphone "Mering" = "MRNK"));
print_endline (string_of_bool (double_metaphone "Merrick" = "MRK"));
print_endline (string_of_bool (double_metaphone "Merry" = "MR"));
print_endline (string_of_bool (double_metaphone "Mighill" = "ML"));
print_endline (string_of_bool (double_metaphone "Miller" = "MLR"));
print_endline (string_of_bool (double_metaphone "Milton" = "MLTN"));
print_endline (string_of_bool (double_metaphone "Mohun" = "MHN"));
print_endline (string_of_bool (double_metaphone "Montague" = "MNTK"));
print_endline (string_of_bool (double_metaphone "Montboucher" = "MNTP"));
print_endline (string_of_bool (double_metaphone "Moore" = "MR"));
print_endline (string_of_bool (double_metaphone "Morrel" = "MRL"));
print_endline (string_of_bool (double_metaphone "Morrill" = "MRL"));
print_endline (string_of_bool (double_metaphone "Morris" = "MRS"));
print_endline (string_of_bool (double_metaphone "Morton" = "MRTN"));
print_endline (string_of_bool (double_metaphone "Moton" = "MTN"));
print_endline (string_of_bool (double_metaphone "Muir" = "MR"));
print_endline (string_of_bool (double_metaphone "Mulferd" = "MLFR"));
print_endline (string_of_bool (double_metaphone "Mullins" = "MLNS"));
print_endline (string_of_bool (double_metaphone "Mulso" = "MLS"));
print_endline (string_of_bool (double_metaphone "Munger" = "MNKR"));
print_endline (string_of_bool (double_metaphone "Munt" = "MNT"));
print_endline (string_of_bool (double_metaphone "Murchad" = "MRXT"));
print_endline (string_of_bool (double_metaphone "Murdock" = "MRTK"));
print_endline (string_of_bool (double_metaphone "Murray" = "MR"));
print_endline (string_of_bool (double_metaphone "Muskett" = "MSKT"));
print_endline (string_of_bool (double_metaphone "Myers" = "MRS"));
print_endline (string_of_bool (double_metaphone "Myrick" = "MRK"));
print_endline (string_of_bool (double_metaphone "NORRIS" = "NRS"));
print_endline (string_of_bool (double_metaphone "Nayle" = "NL"));
print_endline (string_of_bool (double_metaphone "Newcomb" = "NKMP"));
print_endline (string_of_bool (double_metaphone "Newcomb(e)" = "NKMP"));
print_endline (string_of_bool (double_metaphone "Newkirk" = "NKRK"));
print_endline (string_of_bool (double_metaphone "Newton" = "NTN"));
print_endline (string_of_bool (double_metaphone "Niles" = "NLS"));
print_endline (string_of_bool (double_metaphone "Noble" = "NPL"));
print_endline (string_of_bool (double_metaphone "Noel" = "NL"));
print_endline (string_of_bool (double_metaphone "Northend" = "NR0N"));
print_endline (string_of_bool (double_metaphone "Norton" = "NRTN"));
print_endline (string_of_bool (double_metaphone "Nutter" = "NTR"));
print_endline (string_of_bool (double_metaphone "Odding" = "ATNK"));
print_endline (string_of_bool (double_metaphone "Odenbaugh" = "ATNP"));
print_endline (string_of_bool (double_metaphone "Ogborn" = "AKPR"));
print_endline (string_of_bool (double_metaphone "Oppenheimer" = "APNM"));
print_endline (string_of_bool (double_metaphone "Otis" = "ATS"));
print_endline (string_of_bool (double_metaphone "Oviatt" = "AFT"));
print_endline (string_of_bool (double_metaphone "PRUST?" = "PRST"));
print_endline (string_of_bool (double_metaphone "Paddock" = "PTK"));
print_endline (string_of_bool (double_metaphone "Page" = "PJ"));
print_endline (string_of_bool (double_metaphone "Paine" = "PN"));
print_endline (string_of_bool (double_metaphone "Paist" = "PST"));
print_endline (string_of_bool (double_metaphone "Palmer" = "PLMR"));
print_endline (string_of_bool (double_metaphone "Park" = "PRK"));
print_endline (string_of_bool (double_metaphone "Parker" = "PRKR"));
print_endline (string_of_bool (double_metaphone "Parkhurst" = "PRKR"));
print_endline (string_of_bool (double_metaphone "Parrat" = "PRT"));
print_endline (string_of_bool (double_metaphone "Parsons" = "PRSN"));
print_endline (string_of_bool (double_metaphone "Partridge" = "PRTR"));
print_endline (string_of_bool (double_metaphone "Pashley" = "PXL"));
print_endline (string_of_bool (double_metaphone "Pasley" = "PSL"));
print_endline (string_of_bool (double_metaphone "Patrick" = "PTRK"));
print_endline (string_of_bool (double_metaphone "Pattee" = "PT"));
print_endline (string_of_bool (double_metaphone "Patten" = "PTN"));
print_endline (string_of_bool (double_metaphone "Pawley" = "PL"));
print_endline (string_of_bool (double_metaphone "Payne" = "PN"));
print_endline (string_of_bool (double_metaphone "Peabody" = "PPT"));
print_endline (string_of_bool (double_metaphone "Peake" = "PK"));
print_endline (string_of_bool (double_metaphone "Pearson" = "PRSN"));
print_endline (string_of_bool (double_metaphone "Peat" = "PT"));
print_endline (string_of_bool (double_metaphone "Pedersen" = "PTRS"));
print_endline (string_of_bool (double_metaphone "Percy" = "PRS"));
print_endline (string_of_bool (double_metaphone "Perkins" = "PRKN"));
print_endline (string_of_bool (double_metaphone "Perrine" = "PRN"));
print_endline (string_of_bool (double_metaphone "Perry" = "PR"));
print_endline (string_of_bool (double_metaphone "Peson" = "PSN"));
print_endline (string_of_bool (double_metaphone "Peterson" = "PTRS"));
print_endline (string_of_bool (double_metaphone "Peyton" = "PTN"));
print_endline (string_of_bool (double_metaphone "Phinney" = "FN"));
print_endline (string_of_bool (double_metaphone "Pickard" = "PKRT"));
print_endline (string_of_bool (double_metaphone "Pierce" = "PRS"));
print_endline (string_of_bool (double_metaphone "Pierrepont" = "PRPN"));
print_endline (string_of_bool (double_metaphone "Pike" = "PK"));
print_endline (string_of_bool (double_metaphone "Pinkham" = "PNKM"));
print_endline (string_of_bool (double_metaphone "Pitman" = "PTMN"));
print_endline (string_of_bool (double_metaphone "Pitt" = "PT"));
print_endline (string_of_bool (double_metaphone "Pitts" = "PTS"));
print_endline (string_of_bool (double_metaphone "Plantagenet" = "PLNT"));
print_endline (string_of_bool (double_metaphone "Platt" = "PLT"));
print_endline (string_of_bool (double_metaphone "Platts" = "PLTS"));
print_endline (string_of_bool (double_metaphone "Pleis" = "PLS"));
print_endline (string_of_bool (double_metaphone "Pleiss" = "PLS"));
print_endline (string_of_bool (double_metaphone "Plisko" = "PLSK"));
print_endline (string_of_bool (double_metaphone "Pliskovitch" = "PLSK"));
print_endline (string_of_bool (double_metaphone "Plum" = "PLM"));
print_endline (string_of_bool (double_metaphone "Plume" = "PLM"));
print_endline (string_of_bool (double_metaphone "Poitou" = "PT"));
print_endline (string_of_bool (double_metaphone "Pomeroy" = "PMR"));
print_endline (string_of_bool (double_metaphone "Poretiers" = "PRTR"));
print_endline (string_of_bool (double_metaphone "Pote" = "PT"));
print_endline (string_of_bool (double_metaphone "Potter" = "PTR"));
print_endline (string_of_bool (double_metaphone "Potts" = "PTS"));
print_endline (string_of_bool (double_metaphone "Powell" = "PL"));
print_endline (string_of_bool (double_metaphone "Pratt" = "PRT"));
print_endline (string_of_bool (double_metaphone "Presbury" = "PRSP"));
print_endline (string_of_bool (double_metaphone "Priest" = "PRST"));
print_endline (string_of_bool (double_metaphone "Prindle" = "PRNT"));
print_endline (string_of_bool (double_metaphone "Prior" = "PRR"));
print_endline (string_of_bool (double_metaphone "Profumo" = "PRFM"));
print_endline (string_of_bool (double_metaphone "Purdy" = "PRT"));
print_endline (string_of_bool (double_metaphone "Purefoy" = "PRF"));
print_endline (string_of_bool (double_metaphone "Pury" = "PR"));
print_endline (string_of_bool (double_metaphone "Quinter" = "KNTR"));
print_endline (string_of_bool (double_metaphone "Rachel" = "RXL"));
print_endline (string_of_bool (double_metaphone "Rand" = "RNT"));
print_endline (string_of_bool (double_metaphone "Rankin" = "RNKN"));
print_endline (string_of_bool (double_metaphone "Ravenscroft" = "RFNS"));
print_endline (string_of_bool (double_metaphone "Raynsford" = "RNSF"));
print_endline (string_of_bool (double_metaphone "Reakirt" = "RKRT"));
print_endline (string_of_bool (double_metaphone "Reaves" = "RFS"));
print_endline (string_of_bool (double_metaphone "Reeves" = "RFS"));
print_endline (string_of_bool (double_metaphone "Reichert" = "RXRT"));
print_endline (string_of_bool (double_metaphone "Remmele" = "RML"));
print_endline (string_of_bool (double_metaphone "Reynolds" = "RNLT"));
print_endline (string_of_bool (double_metaphone "Rhodes" = "RTS"));
print_endline (string_of_bool (double_metaphone "Richards" = "RXRT"));
print_endline (string_of_bool (double_metaphone "Richardson" = "RXRT"));
print_endline (string_of_bool (double_metaphone "Ring" = "RNK"));
print_endline (string_of_bool (double_metaphone "Roberts" = "RPRT"));
print_endline (string_of_bool (double_metaphone "Robertson" = "RPRT"));
print_endline (string_of_bool (double_metaphone "Robson" = "RPSN"));
print_endline (string_of_bool (double_metaphone "Rodie" = "RT"));
print_endline (string_of_bool (double_metaphone "Rody" = "RT"));
print_endline (string_of_bool (double_metaphone "Rogers" = "RKRS"));
print_endline (string_of_bool (double_metaphone "Ross" = "RS"));
print_endline (string_of_bool (double_metaphone "Rosslevin" = "RSLF"));
print_endline (string_of_bool (double_metaphone "Rowland" = "RLNT"));
print_endline (string_of_bool (double_metaphone "Ruehl" = "RL"));
print_endline (string_of_bool (double_metaphone "Russell" = "RSL"));
print_endline (string_of_bool (double_metaphone "Ruth" = "R0"));
print_endline (string_of_bool (double_metaphone "Ryan" = "RN"));
print_endline (string_of_bool (double_metaphone "Rysse" = "RS"));
print_endline (string_of_bool (double_metaphone "Sadler" = "STLR"));
print_endline (string_of_bool (double_metaphone "Salmon" = "SLMN"));
print_endline (string_of_bool (double_metaphone "Salter" = "SLTR"));
print_endline (string_of_bool (double_metaphone "Salvatore" = "SLFT"));
print_endline (string_of_bool (double_metaphone "Sanders" = "SNTR"));
print_endline (string_of_bool (double_metaphone "Sands" = "SNTS"));
print_endline (string_of_bool (double_metaphone "Sanford" = "SNFR"));
print_endline (string_of_bool (double_metaphone "Sanger" = "SNKR"));
print_endline (string_of_bool (double_metaphone "Sargent" = "SRJN"));
print_endline (string_of_bool (double_metaphone "Saunders" = "SNTR"));
print_endline (string_of_bool (double_metaphone "Schilling" = "XLNK"));
print_endline (string_of_bool (double_metaphone "Schlegel" = "XLKL"));
print_endline (string_of_bool (double_metaphone "Scott" = "SKT"));
print_endline (string_of_bool (double_metaphone "Sears" = "SRS"));
print_endline (string_of_bool (double_metaphone "Segersall" = "SJRS"));
print_endline (string_of_bool (double_metaphone "Senecal" = "SNKL"));
print_endline (string_of_bool (double_metaphone "Sergeaux" = "SRJ"));
print_endline (string_of_bool (double_metaphone "Severance" = "SFRN"));
print_endline (string_of_bool (double_metaphone "Sharp" = "XRP"));
print_endline (string_of_bool (double_metaphone "Sharpe" = "XRP"));
print_endline (string_of_bool (double_metaphone "Sharply" = "XRPL"));
print_endline (string_of_bool (double_metaphone "Shatswell" = "XTSL"));
print_endline (string_of_bool (double_metaphone "Shattack" = "XTK"));
print_endline (string_of_bool (double_metaphone "Shattock" = "XTK"));
print_endline (string_of_bool (double_metaphone "Shattuck" = "XTK"));
print_endline (string_of_bool (double_metaphone "Shaw" = "X"));
print_endline (string_of_bool (double_metaphone "Sheldon" = "XLTN"));
print_endline (string_of_bool (double_metaphone "Sherman" = "XRMN"));
print_endline (string_of_bool (double_metaphone "Shinn" = "XN"));
print_endline (string_of_bool (double_metaphone "Shirford" = "XRFR"));
print_endline (string_of_bool (double_metaphone "Shirley" = "XRL"));
print_endline (string_of_bool (double_metaphone "Shively" = "XFL"));
print_endline (string_of_bool (double_metaphone "Shoemaker" = "XMKR"));
print_endline (string_of_bool (double_metaphone "Short" = "XRT"));
print_endline (string_of_bool (double_metaphone "Shotwell" = "XTL"));
print_endline (string_of_bool (double_metaphone "Shute" = "XT"));
print_endline (string_of_bool (double_metaphone "Sibley" = "SPL"));
print_endline (string_of_bool (double_metaphone "Silver" = "SLFR"));
print_endline (string_of_bool (double_metaphone "Simes" = "SMS"));
print_endline (string_of_bool (double_metaphone "Sinken" = "SNKN"));
print_endline (string_of_bool (double_metaphone "Sinn" = "SN"));
print_endline (string_of_bool (double_metaphone "Skelton" = "SKLT"));
print_endline (string_of_bool (double_metaphone "Skiffe" = "SKF"));
print_endline (string_of_bool (double_metaphone "Skotkonung" = "SKTK"));
print_endline (string_of_bool (double_metaphone "Slade" = "SLT"));
print_endline (string_of_bool (double_metaphone "Slye" = "SL"));
print_endline (string_of_bool (double_metaphone "Smedley" = "SMTL"));
print_endline (string_of_bool (double_metaphone "Smith" = "SM0"));
print_endline (string_of_bool (double_metaphone "Snow" = "SN"));
print_endline (string_of_bool (double_metaphone "Soole" = "SL"));
print_endline (string_of_bool (double_metaphone "Soule" = "SL"));
print_endline (string_of_bool (double_metaphone "Southworth" = "S0R0"));
print_endline (string_of_bool (double_metaphone "Sowles" = "SLS"));
print_endline (string_of_bool (double_metaphone "Spalding" = "SPLT"));
print_endline (string_of_bool (double_metaphone "Spark" = "SPRK"));
print_endline (string_of_bool (double_metaphone "Spencer" = "SPNS"));
print_endline (string_of_bool (double_metaphone "Sperry" = "SPR"));
print_endline (string_of_bool (double_metaphone "Spofford" = "SPFR"));
print_endline (string_of_bool (double_metaphone "Spooner" = "SPNR"));
print_endline (string_of_bool (double_metaphone "Sprague" = "SPRK"));
print_endline (string_of_bool (double_metaphone "Springer" = "SPRN"));
print_endline (string_of_bool (double_metaphone "St. Clair" = "STKL"));
print_endline (string_of_bool (double_metaphone "St. Claire" = "STKL"));
print_endline (string_of_bool (double_metaphone "St. Leger" = "STLJ"));
print_endline (string_of_bool (double_metaphone "St. Omer" = "STMR"));
print_endline (string_of_bool (double_metaphone "Stafferton" = "STFR"));
print_endline (string_of_bool (double_metaphone "Stafford" = "STFR"));
print_endline (string_of_bool (double_metaphone "Stalham" = "STLM"));
print_endline (string_of_bool (double_metaphone "Stanford" = "STNF"));
print_endline (string_of_bool (double_metaphone "Stanton" = "STNT"));
print_endline (string_of_bool (double_metaphone "Star" = "STR"));
print_endline (string_of_bool (double_metaphone "Starbuck" = "STRP"));
print_endline (string_of_bool (double_metaphone "Starkey" = "STRK"));
print_endline (string_of_bool (double_metaphone "Starkweather" = "STRK"));
print_endline (string_of_bool (double_metaphone "Stearns" = "STRN"));
print_endline (string_of_bool (double_metaphone "Stebbins" = "STPN"));
print_endline (string_of_bool (double_metaphone "Steele" = "STL"));
print_endline (string_of_bool (double_metaphone "Stephenson" = "STFN"));
print_endline (string_of_bool (double_metaphone "Stevens" = "STFN"));
print_endline (string_of_bool (double_metaphone "Stoddard" = "STTR"));
print_endline (string_of_bool (double_metaphone "Stodder" = "STTR"));
print_endline (string_of_bool (double_metaphone "Stone" = "STN"));
print_endline (string_of_bool (double_metaphone "Storey" = "STR"));
print_endline (string_of_bool (double_metaphone "Storrada" = "STRT"));
print_endline (string_of_bool (double_metaphone "Story" = "STR"));
print_endline (string_of_bool (double_metaphone "Stoughton" = "STFT"));
print_endline (string_of_bool (double_metaphone "Stout" = "STT"));
print_endline (string_of_bool (double_metaphone "Stow" = "ST"));
print_endline (string_of_bool (double_metaphone "Strong" = "STRN"));
print_endline (string_of_bool (double_metaphone "Strutt" = "STRT"));
print_endline (string_of_bool (double_metaphone "Stryker" = "STRK"));
print_endline (string_of_bool (double_metaphone "Stuckeley" = "STKL"));
print_endline (string_of_bool (double_metaphone "Sturges" = "STRJ"));
print_endline (string_of_bool (double_metaphone "Sturgess" = "STRJ"));
print_endline (string_of_bool (double_metaphone "Sturgis" = "STRJ"));
print_endline (string_of_bool (double_metaphone "Suevain" = "SFN"));
print_endline (string_of_bool (double_metaphone "Sulyard" = "SLRT"));
print_endline (string_of_bool (double_metaphone "Sutton" = "STN"));
print_endline (string_of_bool (double_metaphone "Swain" = "SN"));
print_endline (string_of_bool (double_metaphone "Swayne" = "SN"));
print_endline (string_of_bool (double_metaphone "Swayze" = "SS"));
print_endline (string_of_bool (double_metaphone "Swift" = "SFT"));
print_endline (string_of_bool (double_metaphone "Taber" = "TPR"));
print_endline (string_of_bool (double_metaphone "Talcott" = "TLKT"));
print_endline (string_of_bool (double_metaphone "Tarne" = "TRN"));
print_endline (string_of_bool (double_metaphone "Tatum" = "TTM"));
print_endline (string_of_bool (double_metaphone "Taverner" = "TFRN"));
print_endline (string_of_bool (double_metaphone "Taylor" = "TLR"));
print_endline (string_of_bool (double_metaphone "Tenney" = "TN"));
print_endline (string_of_bool (double_metaphone "Thayer" = "0R"));
print_endline (string_of_bool (double_metaphone "Thember" = "0MPR"));
print_endline (string_of_bool (double_metaphone "Thomas" = "TMS"));
print_endline (string_of_bool (double_metaphone "Thompson" = "TMPS"));
print_endline (string_of_bool (double_metaphone "Thorne" = "0RN"));
print_endline (string_of_bool (double_metaphone "Thornycraft" = "0RNK"));
print_endline (string_of_bool (double_metaphone "Threlkeld" = "0RLK"));
print_endline (string_of_bool (double_metaphone "Throckmorton" = "0RKM"));
print_endline (string_of_bool (double_metaphone "Thwaits" = "0TS"));
print_endline (string_of_bool (double_metaphone "Tibbetts" = "TPTS"));
print_endline (string_of_bool (double_metaphone "Tidd" = "TT"));
print_endline (string_of_bool (double_metaphone "Tierney" = "TRN"));
print_endline (string_of_bool (double_metaphone "Tilley" = "TL"));
print_endline (string_of_bool (double_metaphone "Tillieres" = "TLRS"));
print_endline (string_of_bool (double_metaphone "Tilly" = "TL"));
print_endline (string_of_bool (double_metaphone "Tisdale" = "TSTL"));
print_endline (string_of_bool (double_metaphone "Titus" = "TTS"));
print_endline (string_of_bool (double_metaphone "Tobey" = "TP"));
print_endline (string_of_bool (double_metaphone "Tooker" = "TKR"));
print_endline (string_of_bool (double_metaphone "Towle" = "TL"));
print_endline (string_of_bool (double_metaphone "Towne" = "TN"));
print_endline (string_of_bool (double_metaphone "Townsend" = "TNSN"));
print_endline (string_of_bool (double_metaphone "Treadway" = "TRT"));
print_endline (string_of_bool (double_metaphone "Trelawney" = "TRLN"));
print_endline (string_of_bool (double_metaphone "Trinder" = "TRNT"));
print_endline (string_of_bool (double_metaphone "Tripp" = "TRP"));
print_endline (string_of_bool (double_metaphone "Trippe" = "TRP"));
print_endline (string_of_bool (double_metaphone "Trott" = "TRT"));
print_endline (string_of_bool (double_metaphone "True" = "TR"));
print_endline (string_of_bool (double_metaphone "Trussebut" = "TRSP"));
print_endline (string_of_bool (double_metaphone "Tucker" = "TKR"));
print_endline (string_of_bool (double_metaphone "Turgeon" = "TRJN"));
print_endline (string_of_bool (double_metaphone "Turner" = "TRNR"));
print_endline (string_of_bool (double_metaphone "Tuttle" = "TTL"));
print_endline (string_of_bool (double_metaphone "Tyler" = "TLR"));
print_endline (string_of_bool (double_metaphone "Tylle" = "TL"));
print_endline (string_of_bool (double_metaphone "Tyrrel" = "TRL"));
print_endline (string_of_bool (double_metaphone "Ua Tuathail" = "AT0L"));
print_endline (string_of_bool (double_metaphone "Ulrich" = "ALRX"));
print_endline (string_of_bool (double_metaphone "Underhill" = "ANTR"));
print_endline (string_of_bool (double_metaphone "Underwood" = "ANTR"));
print_endline (string_of_bool (double_metaphone "Unknown" = "ANKN"));
print_endline (string_of_bool (double_metaphone "Valentine" = "FLNT"));
print_endline (string_of_bool (double_metaphone "Van Egmond" = "FNKM"));
print_endline (string_of_bool (double_metaphone "Van der Beek" = "FNTR"));
print_endline (string_of_bool (double_metaphone "Vaughan" = "FKN"));
print_endline (string_of_bool (double_metaphone "Vermenlen" = "FRMN"));
print_endline (string_of_bool (double_metaphone "Vincent" = "FNSN"));
print_endline (string_of_bool (double_metaphone "Volentine" = "FLNT"));
print_endline (string_of_bool (double_metaphone "Wagner" = "AKNR")); (* !!! *)
print_endline (string_of_bool (double_metaphone "Waite" = "AT"));
print_endline (string_of_bool (double_metaphone "Walker" = "ALKR"));
print_endline (string_of_bool (double_metaphone "Walter" = "ALTR"));
print_endline (string_of_bool (double_metaphone "Wandell" = "ANTL"));
print_endline (string_of_bool (double_metaphone "Wandesford" = "ANTS"));
print_endline (string_of_bool (double_metaphone "Warbleton" = "ARPL"));
print_endline (string_of_bool (double_metaphone "Ward" = "ART"));
print_endline (string_of_bool (double_metaphone "Warde" = "ART"));
print_endline (string_of_bool (double_metaphone "Ware" = "AR"));
print_endline (string_of_bool (double_metaphone "Wareham" = "ARHM"));
print_endline (string_of_bool (double_metaphone "Warner" = "ARNR"));
print_endline (string_of_bool (double_metaphone "Warren" = "ARN"));
print_endline (string_of_bool (double_metaphone "Washburne" = "AXPR"));
print_endline (string_of_bool (double_metaphone "Waterbury" = "ATRP"));
print_endline (string_of_bool (double_metaphone "Watson" = "ATSN"));
print_endline (string_of_bool (double_metaphone "WatsonEllithorpe" = "ATSN"));
print_endline (string_of_bool (double_metaphone "Watts" = "ATS"));
print_endline (string_of_bool (double_metaphone "Wayne" = "AN"));
print_endline (string_of_bool (double_metaphone "Webb" = "AP"));
print_endline (string_of_bool (double_metaphone "Weber" = "APR"));
print_endline (string_of_bool (double_metaphone "Webster" = "APST"));
print_endline (string_of_bool (double_metaphone "Weed" = "AT"));
print_endline (string_of_bool (double_metaphone "Weeks" = "AKS"));
print_endline (string_of_bool (double_metaphone "Wells" = "ALS"));
print_endline (string_of_bool (double_metaphone "Wenzell" = "ANSL"));
print_endline (string_of_bool (double_metaphone "West" = "AST"));
print_endline (string_of_bool (double_metaphone "Westbury" = "ASTP"));
print_endline (string_of_bool (double_metaphone "Whatlocke" = "ATLK"));
print_endline (string_of_bool (double_metaphone "Wheeler" = "ALR"));
print_endline (string_of_bool (double_metaphone "Whiston" = "ASTN"));
print_endline (string_of_bool (double_metaphone "White" = "AT"));
print_endline (string_of_bool (double_metaphone "Whitman" = "ATMN"));
print_endline (string_of_bool (double_metaphone "Whiton" = "ATN"));
print_endline (string_of_bool (double_metaphone "Whitson" = "ATSN"));
print_endline (string_of_bool (double_metaphone "Wickes" = "AKS"));
print_endline (string_of_bool (double_metaphone "Wilbur" = "ALPR"));
print_endline (string_of_bool (double_metaphone "Wilcotes" = "ALKT"));
print_endline (string_of_bool (double_metaphone "Wilkinson" = "ALKN"));
print_endline (string_of_bool (double_metaphone "Willets" = "ALTS"));
print_endline (string_of_bool (double_metaphone "Willett" = "ALT"));
print_endline (string_of_bool (double_metaphone "Willey" = "AL"));
print_endline (string_of_bool (double_metaphone "Williams" = "ALMS"));
print_endline (string_of_bool (double_metaphone "Williston" = "ALST"));
print_endline (string_of_bool (double_metaphone "Wilson" = "ALSN"));
print_endline (string_of_bool (double_metaphone "Wimes" = "AMS"));
print_endline (string_of_bool (double_metaphone "Winch" = "ANX"));
print_endline (string_of_bool (double_metaphone "Winegar" = "ANKR"));
print_endline (string_of_bool (double_metaphone "Wing" = "ANK"));
print_endline (string_of_bool (double_metaphone "Winsley" = "ANSL"));
print_endline (string_of_bool (double_metaphone "Winslow" = "ANSL"));
print_endline (string_of_bool (double_metaphone "Winthrop" = "AN0R"));
print_endline (string_of_bool (double_metaphone "Wise" = "AS"));
print_endline (string_of_bool (double_metaphone "Wood" = "AT"));
print_endline (string_of_bool (double_metaphone "Woodbridge" = "ATPR"));
print_endline (string_of_bool (double_metaphone "Woodward" = "ATRT"));
print_endline (string_of_bool (double_metaphone "Wooley" = "AL"));
print_endline (string_of_bool (double_metaphone "Woolley" = "AL"));
print_endline (string_of_bool (double_metaphone "Worth" = "AR0"));
print_endline (string_of_bool (double_metaphone "Worthen" = "AR0N"));
print_endline (string_of_bool (double_metaphone "Worthley" = "AR0L"));
print_endline (string_of_bool (double_metaphone "Wright" = "RT"));
print_endline (string_of_bool (double_metaphone "Wyer" = "AR"));
print_endline (string_of_bool (double_metaphone "Wyere" = "AR"));
print_endline (string_of_bool (double_metaphone "Wynkoop" = "ANKP"));
print_endline (string_of_bool (double_metaphone "Yarnall" = "ARNL"));
print_endline (string_of_bool (double_metaphone "Yeoman" = "AMN"));
print_endline (string_of_bool (double_metaphone "Yorke" = "ARK"));
print_endline (string_of_bool (double_metaphone "Young" = "ANK"));
print_endline (string_of_bool (double_metaphone "ab Wennonwen" = "APNN"));
print_endline (string_of_bool (double_metaphone "ap Llewellyn" = "APLL"));
print_endline (string_of_bool (double_metaphone "ap Lorwerth" = "APLR"));
print_endline (string_of_bool (double_metaphone "d'Angouleme" = "TNKL"));
print_endline (string_of_bool (double_metaphone "de Audeham" = "TTHM"));
print_endline (string_of_bool (double_metaphone "de Bavant" = "TPFN"));
print_endline (string_of_bool (double_metaphone "de Beauchamp" = "TPXM"));
print_endline (string_of_bool (double_metaphone "de Beaumont" = "TPMN"));
print_endline (string_of_bool (double_metaphone "de Bolbec" = "TPLP"));
print_endline (string_of_bool (double_metaphone "de Braiose" = "TPRS"));
print_endline (string_of_bool (double_metaphone "de Braose" = "TPRS"));
print_endline (string_of_bool (double_metaphone "de Briwere" = "TPRR"));
print_endline (string_of_bool (double_metaphone "de Cantelou" = "TKNT"));
print_endline (string_of_bool (double_metaphone "de Cherelton" = "TXRL"));
print_endline (string_of_bool (double_metaphone "de Cherleton" = "TXRL"));
print_endline (string_of_bool (double_metaphone "de Clare" = "TKLR"));
print_endline (string_of_bool (double_metaphone "de Claremont" = "TKLR"));
print_endline (string_of_bool (double_metaphone "de Clifford" = "TKLF"));
print_endline (string_of_bool (double_metaphone "de Colville" = "TKLF"));
print_endline (string_of_bool (double_metaphone "de Courtenay" = "TKRT"));
print_endline (string_of_bool (double_metaphone "de Fauconberg" = "TFKN"));
print_endline (string_of_bool (double_metaphone "de Forest" = "TFRS"));
print_endline (string_of_bool (double_metaphone "de Gai" = "TK"));
print_endline (string_of_bool (double_metaphone "de Grey" = "TKR"));
print_endline (string_of_bool (double_metaphone "de Guernons" = "TKRN"));
print_endline (string_of_bool (double_metaphone "de Haia" = "T"));
print_endline (string_of_bool (double_metaphone "de Harcourt" = "TRKR"));
print_endline (string_of_bool (double_metaphone "de Hastings" = "TSTN"));
print_endline (string_of_bool (double_metaphone "de Hoke" = "TK"));
print_endline (string_of_bool (double_metaphone "de Hooch" = "TK"));
print_endline (string_of_bool (double_metaphone "de Hugelville" = "TJLF"));
print_endline (string_of_bool (double_metaphone "de Huntingdon" = "TNTN"));
print_endline (string_of_bool (double_metaphone "de Insula" = "TNSL"));
print_endline (string_of_bool (double_metaphone "de Keynes" = "TKNS"));
print_endline (string_of_bool (double_metaphone "de Lacy" = "TLS"));
print_endline (string_of_bool (double_metaphone "de Lexington" = "TLKS"));
print_endline (string_of_bool (double_metaphone "de Lusignan" = "TLSN"));
print_endline (string_of_bool (double_metaphone "de Manvers" = "TMNF"));
print_endline (string_of_bool (double_metaphone "de Montagu" = "TMNT"));
print_endline (string_of_bool (double_metaphone "de Montault" = "TMNT"));
print_endline (string_of_bool (double_metaphone "de Montfort" = "TMNT"));
print_endline (string_of_bool (double_metaphone "de Mortimer" = "TMRT"));
print_endline (string_of_bool (double_metaphone "de Morville" = "TMRF"));
print_endline (string_of_bool (double_metaphone "de Morvois" = "TMRF"));
print_endline (string_of_bool (double_metaphone "de Neufmarche" = "TNFM"));
print_endline (string_of_bool (double_metaphone "de Odingsells" = "TTNK"));
print_endline (string_of_bool (double_metaphone "de Odyngsells" = "TTNK"));
print_endline (string_of_bool (double_metaphone "de Percy" = "TPRS"));
print_endline (string_of_bool (double_metaphone "de Pierrepont" = "TPRP"));
print_endline (string_of_bool (double_metaphone "de Plessetis" = "TPLS"));
print_endline (string_of_bool (double_metaphone "de Porhoet" = "TPRT"));
print_endline (string_of_bool (double_metaphone "de Prouz" = "TPRS"));
print_endline (string_of_bool (double_metaphone "de Quincy" = "TKNS"));
print_endline (string_of_bool (double_metaphone "de Ripellis" = "TRPL"));
print_endline (string_of_bool (double_metaphone "de Ros" = "TRS"));
print_endline (string_of_bool (double_metaphone "de Salisbury" = "TSLS"));
print_endline (string_of_bool (double_metaphone "de Sanford" = "TSNF"));
print_endline (string_of_bool (double_metaphone "de Somery" = "TSMR"));
print_endline (string_of_bool (double_metaphone "de St. Hilary" = "TSTL"));
print_endline (string_of_bool (double_metaphone "de St. Liz" = "TSTL"));
print_endline (string_of_bool (double_metaphone "de Sutton" = "TSTN"));
print_endline (string_of_bool (double_metaphone "de Toeni" = "TTN"));
print_endline (string_of_bool (double_metaphone "de Tony" = "TTN"));
print_endline (string_of_bool (double_metaphone "de Umfreville" = "TMFR"));
print_endline (string_of_bool (double_metaphone "de Valognes" = "TFLN"));
print_endline (string_of_bool (double_metaphone "de Vaux" = "TF"));
print_endline (string_of_bool (double_metaphone "de Vere" = "TFR"));
print_endline (string_of_bool (double_metaphone "de Vermandois" = "TFRM"));
print_endline (string_of_bool (double_metaphone "de Vernon" = "TFRN"));
print_endline (string_of_bool (double_metaphone "de Vexin" = "TFKS"));
print_endline (string_of_bool (double_metaphone "de Vitre" = "TFTR"));
print_endline (string_of_bool (double_metaphone "de Wandesford" = "TNTS"));
print_endline (string_of_bool (double_metaphone "de Warenne" = "TRN"));
print_endline (string_of_bool (double_metaphone "de Westbury" = "TSTP"));
print_endline (string_of_bool (double_metaphone "di Saluzzo" = "TSLS"));
print_endline (string_of_bool (double_metaphone "fitz Alan" = "FTSL"));
print_endline (string_of_bool (double_metaphone "fitz Geoffrey" = "FTSJ"));
print_endline (string_of_bool (double_metaphone "fitz Herbert" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz John" = "FTSJ"));
print_endline (string_of_bool (double_metaphone "fitz Patrick" = "FTSP"));
print_endline (string_of_bool (double_metaphone "fitz Payn" = "FTSP"));
print_endline (string_of_bool (double_metaphone "fitz Piers" = "FTSP"));
print_endline (string_of_bool (double_metaphone "fitz Randolph" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz Richard" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz Robert" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz Roy" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz Scrob" = "FTSS"));
print_endline (string_of_bool (double_metaphone "fitz Walter" = "FTSL"));
print_endline (string_of_bool (double_metaphone "fitz Warin" = "FTSR"));
print_endline (string_of_bool (double_metaphone "fitz Williams" = "FTSL"));
print_endline (string_of_bool (double_metaphone "la Zouche" = "LSX"));
print_endline (string_of_bool (double_metaphone "le Botiller" = "LPTL"));
print_endline (string_of_bool (double_metaphone "le Despenser" = "LTSP"));
print_endline (string_of_bool (double_metaphone "le deSpencer" = "LTSP"));
print_endline (string_of_bool (double_metaphone "of Allendale" = "AFLN"));
print_endline (string_of_bool (double_metaphone "of Angouleme" = "AFNK"));
print_endline (string_of_bool (double_metaphone "of Anjou" = "AFNJ"));
print_endline (string_of_bool (double_metaphone "of Aquitaine" = "AFKT"));
print_endline (string_of_bool (double_metaphone "of Aumale" = "AFML"));
print_endline (string_of_bool (double_metaphone "of Bavaria" = "AFPF"));
print_endline (string_of_bool (double_metaphone "of Boulogne" = "AFPL"));
print_endline (string_of_bool (double_metaphone "of Brittany" = "AFPR"));
print_endline (string_of_bool (double_metaphone "of Brittary" = "AFPR"));
print_endline (string_of_bool (double_metaphone "of Castile" = "AFKS"));
print_endline (string_of_bool (double_metaphone "of Chester" = "AFXS"));
print_endline (string_of_bool (double_metaphone "of Clermont" = "AFKL"));
print_endline (string_of_bool (double_metaphone "of Cologne" = "AFKL"));
print_endline (string_of_bool (double_metaphone "of Dinan" = "AFTN"));
print_endline (string_of_bool (double_metaphone "of Dunbar" = "AFTN"));
print_endline (string_of_bool (double_metaphone "of England" = "AFNK"));
print_endline (string_of_bool (double_metaphone "of Essex" = "AFSK"));
print_endline (string_of_bool (double_metaphone "of Falaise" = "AFFL"));
print_endline (string_of_bool (double_metaphone "of Flanders" = "AFFL"));
print_endline (string_of_bool (double_metaphone "of Galloway" = "AFKL"));
print_endline (string_of_bool (double_metaphone "of Germany" = "AFKR"));
print_endline (string_of_bool (double_metaphone "of Gloucester" = "AFKL"));
print_endline (string_of_bool (double_metaphone "of Heristal" = "AFRS"));
print_endline (string_of_bool (double_metaphone "of Hungary" = "AFNK"));
print_endline (string_of_bool (double_metaphone "of Huntington" = "AFNT"));
print_endline (string_of_bool (double_metaphone "of Kiev" = "AFKF"));
print_endline (string_of_bool (double_metaphone "of Kuno" = "AFKN"));
print_endline (string_of_bool (double_metaphone "of Landen" = "AFLN"));
print_endline (string_of_bool (double_metaphone "of Laon" = "AFLN"));
print_endline (string_of_bool (double_metaphone "of Leinster" = "AFLN"));
print_endline (string_of_bool (double_metaphone "of Lens" = "AFLN"));
print_endline (string_of_bool (double_metaphone "of Lorraine" = "AFLR"));
print_endline (string_of_bool (double_metaphone "of Louvain" = "AFLF"));
print_endline (string_of_bool (double_metaphone "of Mercia" = "AFMR"));
print_endline (string_of_bool (double_metaphone "of Metz" = "AFMT"));
print_endline (string_of_bool (double_metaphone "of Meulan" = "AFML"));
print_endline (string_of_bool (double_metaphone "of Nass" = "AFNS"));
print_endline (string_of_bool (double_metaphone "of Normandy" = "AFNR"));
print_endline (string_of_bool (double_metaphone "of Ohningen" = "AFNN"));
print_endline (string_of_bool (double_metaphone "of Orleans" = "AFRL"));
print_endline (string_of_bool (double_metaphone "of Poitou" = "AFPT"));
print_endline (string_of_bool (double_metaphone "of Polotzk" = "AFPL"));
print_endline (string_of_bool (double_metaphone "of Provence" = "AFPR"));
print_endline (string_of_bool (double_metaphone "of Ringelheim" = "AFRN"));
print_endline (string_of_bool (double_metaphone "of Salisbury" = "AFSL"));
print_endline (string_of_bool (double_metaphone "of Saxony" = "AFSK"));
print_endline (string_of_bool (double_metaphone "of Scotland" = "AFSK"));
print_endline (string_of_bool (double_metaphone "of Senlis" = "AFSN"));
print_endline (string_of_bool (double_metaphone "of Stafford" = "AFST"));
print_endline (string_of_bool (double_metaphone "of Swabia" = "AFSP"));
print_endline (string_of_bool (double_metaphone "of Tongres" = "AFTN"));
print_endline (string_of_bool (double_metaphone "of the Tributes" = "AF0T"));
print_endline (string_of_bool (double_metaphone "unknown" = "ANKN"));
print_endline (string_of_bool (double_metaphone "van der Gouda" = "FNTR"));
print_endline (string_of_bool (double_metaphone "von Adenbaugh" = "FNTN"));
print_endline (string_of_bool (double_metaphone "ARCHITure" = "ARKT"));
print_endline (string_of_bool (double_metaphone "Arnoff" = "ARNF"));
print_endline (string_of_bool (double_metaphone "Arnow" = "ARN"));
print_endline (string_of_bool (double_metaphone "DANGER" = "TNJR"));
print_endline (string_of_bool (double_metaphone "Jankelowicz" = "JNKL"));
print_endline (string_of_bool (double_metaphone "MANGER" = "MNJR"));
print_endline (string_of_bool (double_metaphone "McClellan" = "MKLL"));
print_endline (string_of_bool (double_metaphone "McHugh" = "MK"));
print_endline (string_of_bool (double_metaphone "McLaughlin" = "MKLF"));
print_endline (string_of_bool (double_metaphone "ORCHEStra" = "ARKS"));
print_endline (string_of_bool (double_metaphone "ORCHID" = "ARKT"));
print_endline (string_of_bool (double_metaphone "Pierce" = "PRS"));
print_endline (string_of_bool (double_metaphone "RANGER" = "RNJR"));
print_endline (string_of_bool (double_metaphone "Schlesinger" = "XLSN"));
print_endline (string_of_bool (double_metaphone "Uomo" = "AM"));
print_endline (string_of_bool (double_metaphone "Vasserman" = "FSRM"));
print_endline (string_of_bool (double_metaphone "Wasserman" = "ASRM"));
print_endline (string_of_bool (double_metaphone "Womo" = "AM"));
print_endline (string_of_bool (double_metaphone "Yankelovich" = "ANKL"));
print_endline (string_of_bool (double_metaphone "accede" = "AKST"));
print_endline (string_of_bool (double_metaphone "accident" = "AKST"));
print_endline (string_of_bool (double_metaphone "adelsheim" = "ATLS"));
print_endline (string_of_bool (double_metaphone "aged" = "AJT"));
print_endline (string_of_bool (double_metaphone "ageless" = "AJLS"));
print_endline (string_of_bool (double_metaphone "agency" = "AJNS"));
print_endline (string_of_bool (double_metaphone "aghast" = "AKST"));
print_endline (string_of_bool (double_metaphone "agio" = "AJ"));
print_endline (string_of_bool (double_metaphone "agrimony" = "AKRM"));
print_endline (string_of_bool (double_metaphone "album" = "ALPM"));
print_endline (string_of_bool (double_metaphone "alcmene" = "ALKM"));
print_endline (string_of_bool (double_metaphone "alehouse" = "ALHS"));
print_endline (string_of_bool (double_metaphone "antique" = "ANTK"));
print_endline (string_of_bool (double_metaphone "artois" = "ART"));
print_endline (string_of_bool (double_metaphone "automation" = "ATMX"));
print_endline (string_of_bool (double_metaphone "bacchus" = "PKS"));
print_endline (string_of_bool (double_metaphone "bacci" = "PX"));
print_endline (string_of_bool (double_metaphone "bajador" = "PJTR"));
print_endline (string_of_bool (double_metaphone "bellocchio" = "PLX"));
print_endline (string_of_bool (double_metaphone "bertucci" = "PRTX"));
print_endline (string_of_bool (double_metaphone "biaggi" = "PJ"));
print_endline (string_of_bool (double_metaphone "bough" = "P"));
print_endline (string_of_bool (double_metaphone "breaux" = "PR"));
print_endline (string_of_bool (double_metaphone "broughton" = "PRTN"));
print_endline (string_of_bool (double_metaphone "cabrillo" = "KPRL"));
print_endline (string_of_bool (double_metaphone "caesar" = "SSR"));
print_endline (string_of_bool (double_metaphone "cagney" = "KKN"));
print_endline (string_of_bool (double_metaphone "campbell" = "KMPL"));
print_endline (string_of_bool (double_metaphone "carlisle" = "KRLL"));
print_endline (string_of_bool (double_metaphone "carlysle" = "KRLL"));
print_endline (string_of_bool (double_metaphone "chemistry" = "KMST"));
print_endline (string_of_bool (double_metaphone "chianti" = "KNT"));
print_endline (string_of_bool (double_metaphone "chorus" = "KRS"));
print_endline (string_of_bool (double_metaphone "cough" = "KF"));
print_endline (string_of_bool (double_metaphone "czerny" = "SRN"));
print_endline (string_of_bool (double_metaphone "deffenbacher" = "TFNP"));
print_endline (string_of_bool (double_metaphone "dumb" = "TM"));
print_endline (string_of_bool (double_metaphone "edgar" = "ATKR"));
print_endline (string_of_bool (double_metaphone "edge" = "AJ"));
print_endline (string_of_bool (double_metaphone "filipowicz" = "FLPT"));
print_endline (string_of_bool (double_metaphone "focaccia" = "FKX"));
print_endline (string_of_bool (double_metaphone "gallegos" = "KLKS"));
print_endline (string_of_bool (double_metaphone "gambrelli" = "KMPR"));
print_endline (string_of_bool (double_metaphone "geithain" = "K0N"));
print_endline (string_of_bool (double_metaphone "ghiradelli" = "JRTL"));
print_endline (string_of_bool (double_metaphone "ghislane" = "JLN"));
print_endline (string_of_bool (double_metaphone "gough" = "KF"));
print_endline (string_of_bool (double_metaphone "hartheim" = "HR0M"));
print_endline (string_of_bool (double_metaphone "heimsheim" = "HMSM"));
print_endline (string_of_bool (double_metaphone "hochmeier" = "HKMR"));
print_endline (string_of_bool (double_metaphone "hugh" = "H"));
print_endline (string_of_bool (double_metaphone "hunger" = "HNKR"));
print_endline (string_of_bool (double_metaphone "hungry" = "HNKR"));
print_endline (string_of_bool (double_metaphone "island" = "ALNT"));
print_endline (string_of_bool (double_metaphone "isle" = "AL"));
print_endline (string_of_bool (double_metaphone "jose" = "HS"));
print_endline (string_of_bool (double_metaphone "laugh" = "LF"));
print_endline (string_of_bool (double_metaphone "mac caffrey" = "MKFR"));
print_endline (string_of_bool (double_metaphone "mac gregor" = "MKRK"));
print_endline (string_of_bool (double_metaphone "pegnitz" = "PNTS"));
print_endline (string_of_bool (double_metaphone "piskowitz" = "PSKT"));
print_endline (string_of_bool (double_metaphone "queen" = "KN"));
print_endline (string_of_bool (double_metaphone "raspberry" = "RSPR"));
print_endline (string_of_bool (double_metaphone "resnais" = "RSN"));
print_endline (string_of_bool (double_metaphone "rogier" = "RJ")); (* !!! *)
print_endline (string_of_bool (double_metaphone "rough" = "RF"));
print_endline (string_of_bool (double_metaphone "san jacinto" = "SNHS"));
print_endline (string_of_bool (double_metaphone "schenker" = "XNKR"));
print_endline (string_of_bool (double_metaphone "schermerhorn" = "XRMR"));
print_endline (string_of_bool (double_metaphone "schmidt" = "XMT"));
print_endline (string_of_bool (double_metaphone "schneider" = "XNTR"));
print_endline (string_of_bool (double_metaphone "school" = "SKL"));
print_endline (string_of_bool (double_metaphone "schooner" = "SKNR"));
print_endline (string_of_bool (double_metaphone "schrozberg" = "XRSP"));
print_endline (string_of_bool (double_metaphone "schulman" = "XLMN"));
print_endline (string_of_bool (double_metaphone "schwabach" = "XPK"));
print_endline (string_of_bool (double_metaphone "schwarzach" = "XRSK"));
print_endline (string_of_bool (double_metaphone "smith" = "SM0"));
print_endline (string_of_bool (double_metaphone "snider" = "SNTR"));
print_endline (string_of_bool (double_metaphone "succeed" = "SKST"));
print_endline (string_of_bool (double_metaphone "sugarcane" = "XKRK"));
print_endline (string_of_bool (double_metaphone "svobodka" = "SFPT"));
print_endline (string_of_bool (double_metaphone "tagliaro" = "TKLR"));
print_endline (string_of_bool (double_metaphone "thames" = "TMS"));
print_endline (string_of_bool (double_metaphone "theilheim" = "0LM"));
print_endline (string_of_bool (double_metaphone "thomas" = "TMS"));
print_endline (string_of_bool (double_metaphone "thumb" = "0M"));
print_endline (string_of_bool (double_metaphone "tichner" = "TXNR"));
print_endline (string_of_bool (double_metaphone "tough" = "TF"));
print_endline (string_of_bool (double_metaphone "umbrella" = "AMPR"));
print_endline (string_of_bool (double_metaphone "vilshofen" = "FLXF"));
print_endline (string_of_bool (double_metaphone "von schuller" = "FNXL"));
print_endline (string_of_bool (double_metaphone "wachtler" = "AKTL"));
print_endline (string_of_bool (double_metaphone "wechsler" = "AKSL"));
print_endline (string_of_bool (double_metaphone "weikersheim" = "AKRS"));
print_endline (string_of_bool (double_metaphone "zhao" = "J"));
*)

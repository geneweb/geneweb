(* camlp5r *)
(* $Id: templDate.ml,v 5.3 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config;
open Def;
open Mutil;
open Printf;
open TemplAst;
open Util;

value rec eval_date_var conf jd =
  fun
  [ ["french" :: sl] -> eval_dmy_var (Calendar.french_of_sdn Sure jd) sl
  | ["gregorian" :: sl] -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl
  | ["hebrew" :: sl] -> eval_dmy_var (Calendar.hebrew_of_sdn Sure jd) sl
  | ["julian" :: sl] -> eval_dmy_var (Calendar.julian_of_sdn Sure jd) sl
  | ["julian_day"] -> VVstring (string_of_int jd)
  | ["julian_day"; "sep1000"] ->
       VVstring
         (Num.to_string_sep (transl conf "(thousand separator)")
            (Num.of_int jd))
  | ["moon_age"] ->
        try
          let (mp, md) = Calendar.moon_phase_of_sdn jd in
          VVstring (string_of_int md)
        with [ Failure _ -> VVstring "" ]
  | ["moon_phase" :: sl] ->
        try
          let (mp, md) = Calendar.moon_phase_of_sdn jd in
          eval_moon_phase_var mp sl
        with [ Failure _ -> VVstring "" ]
  | ["week_day"] ->
      let wday =
        let jd_today = Calendar.sdn_of_gregorian conf.today in
        let x = conf.today_wd - jd_today + jd in
        if x < 0 then 6 + (x + 1) mod 7 else x mod 7
      in
      VVstring (string_of_int wday)
  | sl -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl ]
and eval_moon_phase_var mp =
  fun
  [ ["hour"] ->
      let s =
        match mp with
        [ None -> ""
        | Some (_, hh, _) -> sprintf "%02d" hh ]
      in
      VVstring s
  | ["index"] ->
      let i =
        match mp with
        [ None -> 0
        | Some (Calendar.NewMoon, _, _) -> 1
        | Some (Calendar.FirstQuarter, _, _) -> 2
        | Some (Calendar.FullMoon, _, _) -> 3
        | Some (Calendar.LastQuarter, _, _) -> 4 ]
      in
      VVstring (string_of_int i)
  | ["minute"] ->
      let s =
        match mp with
        [ None -> ""
        | Some (_, _, mm) -> sprintf "%02d" mm ]
      in
      VVstring s
  | _ -> raise Not_found ]
and eval_dmy_var dmy =
  fun
  [ ["day"] -> VVstring (string_of_int dmy.day)
  | ["month"] -> VVstring (string_of_int dmy.month)
  | ["year" :: sl] -> eval_integer dmy.year sl
  | [] -> VVstring (sprintf "%d-%02d-%02d" dmy.year dmy.month dmy.day)
  | _ -> raise Not_found ]
and eval_integer i =
  fun
  [ ["roman"] -> VVstring (roman_of_arabian i)
  | [] -> VVstring (string_of_int i)
  | _ -> raise Not_found ]
;

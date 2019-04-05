(* $Id: templDate.ml,v 5.3 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 2007 INRIA *)

open Config
open Def
open TemplAst
open Util

let rec eval_date_var conf jd =
  function
    "french" :: sl -> eval_dmy_var (Calendar.french_of_sdn Sure jd) sl
  | "gregorian" :: sl -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl
  | "hebrew" :: sl -> eval_dmy_var (Calendar.hebrew_of_sdn Sure jd) sl
  | "julian" :: sl -> eval_dmy_var (Calendar.julian_of_sdn Sure jd) sl
  | ["julian_day"] -> VVstring (string_of_int jd)
  | ["julian_day"; "sep1000"] ->
      VVstring
        (Mutil.string_of_int_sep (transl conf "(thousand separator)") jd)
  | ["moon_age"] ->
      begin try
        let (_, md) = Calendar.moon_phase_of_sdn jd in
        VVstring (string_of_int md)
      with Failure _ -> VVstring ""
      end
  | "moon_phase" :: sl ->
      begin try
        let (mp, _) = Calendar.moon_phase_of_sdn jd in
        eval_moon_phase_var mp sl
      with Failure _ -> VVstring ""
      end
  | ["week_day"] ->
      let wday =
        let jd_today = Calendar.sdn_of_gregorian conf.today in
        let x = conf.today_wd - jd_today + jd in
        if x < 0 then 6 + (x + 1) mod 7 else x mod 7
      in
      VVstring (string_of_int wday)
  | sl -> eval_dmy_var (Calendar.gregorian_of_sdn Sure jd) sl
and eval_moon_phase_var mp =
  function
    ["hour"] ->
      let s =
        match mp with
          None -> ""
        | Some (_, hh, _) -> Printf.sprintf "%02d" hh
      in
      VVstring s
  | ["index"] ->
      let i =
        match mp with
          None -> 0
        | Some (Calendar.NewMoon, _, _) -> 1
        | Some (Calendar.FirstQuarter, _, _) -> 2
        | Some (Calendar.FullMoon, _, _) -> 3
        | Some (Calendar.LastQuarter, _, _) -> 4
      in
      VVstring (string_of_int i)
  | ["minute"] ->
      let s =
        match mp with
          None -> ""
        | Some (_, _, mm) -> Printf.sprintf "%02d" mm
      in
      VVstring s
  | _ -> raise Not_found
and eval_dmy_var dmy =
  function
    ["day"] -> VVstring (string_of_int dmy.day)
  | ["month"] -> VVstring (string_of_int dmy.month)
  | "year" :: sl -> eval_integer dmy.year sl
  | [] -> VVstring (Printf.sprintf "%d-%02d-%02d" dmy.year dmy.month dmy.day)
  | _ -> raise Not_found
and eval_integer i =
  function
    ["roman"] -> VVstring (Mutil.roman_of_arabian i)
  | [] -> VVstring (string_of_int i)
  | _ -> raise Not_found

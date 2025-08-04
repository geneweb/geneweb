open Config
open Def

type 'a env_value = Vint of int | Vother of 'a | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x

let eval_julian_day conf =
  let getint v = match Util.p_getint conf.env v with Some x -> x | _ -> 0 in

  let process_calendar (var, cal, conv, max_month) =
    let yy =
      match Util.p_getenv conf.env ("y" ^ var) with
      | Some v -> (
          try
            let len = String.length v in
            if cal = Djulian && len > 2 && v.[len - 2] = '/' then
              int_of_string (String.sub v 0 (len - 2)) + 1
            else int_of_string v
          with Failure _ -> 0)
      | None -> 0
    in
    let mm = getint ("m" ^ var) in
    let dd = getint ("d" ^ var) in
    let dt = { day = dd; month = mm; year = yy; prec = Sure; delta = 0 } in

    match Util.p_getenv conf.env ("t" ^ var) with
    | Some _ -> Some (conv dt)
    | None ->
        let check_env suffix = Util.p_getenv conf.env suffix <> None in

        if check_env ("y" ^ var ^ "1") then
          Some (conv { dt with year = yy - 1 })
        else if check_env ("y" ^ var ^ "2") then
          Some (conv { dt with year = yy + 1 })
        else if check_env ("m" ^ var ^ "1") then
          let yy, mm = if mm = 1 then (yy - 1, max_month) else (yy, mm - 1) in
          Some (conv { dt with year = yy; month = mm })
        else if check_env ("m" ^ var ^ "2") then
          let yy, mm = if mm = max_month then (yy + 1, 1) else (yy, mm + 1) in
          let r = conv { dt with year = yy; month = mm } in
          if r = conv dt then
            let yy, mm = if mm = max_month then (yy + 1, 1) else (yy, mm + 1) in
            Some (conv { dt with year = yy; month = mm })
          else Some r
        else if check_env ("d" ^ var ^ "1") then
          Some (conv { dt with day = dd - 1 })
        else if check_env ("d" ^ var ^ "2") then
          Some (conv { dt with day = dd + 1 })
        else None
  in

  let calendars =
    [
      ("g", Dgregorian, Calendar.sdn_of_gregorian, 12);
      ("j", Djulian, Calendar.sdn_of_julian, 12);
      ("f", Dfrench, Calendar.sdn_of_french, 13);
      ("h", Dhebrew, Calendar.sdn_of_hebrew, 13);
    ]
  in

  List.fold_left
    (fun acc cal ->
      if acc <> Calendar.sdn_of_gregorian conf.today then acc
      else match process_calendar cal with Some jd -> jd | None -> acc)
    (Calendar.sdn_of_gregorian conf.today)
    calendars

let eval_var conf jd env _xx _loc = function
  | [ "integer" ] -> (
      match get_env "integer" env with
      | Vint i -> Templ.VVstring (string_of_int i)
      | _ -> raise Not_found)
  | "date" :: sl -> Templ.eval_date_var conf jd sl
  | "today" :: sl ->
      Templ.eval_date_var conf (Calendar.sdn_of_gregorian conf.today) sl
  | _ -> raise Not_found

let print_foreach _conf _jd print_ast eval_expr env _xx _loc s sl el al =
  match (s, sl) with
  | "integer_range", [] ->
      let eval_int_expr ast_list =
        match ast_list with
        | [ ast ] -> (
            let s = eval_expr env () ast in
            try int_of_string s with Failure _ -> raise Not_found)
        | _ -> raise Not_found
      in
      let i1, i2 =
        match el with
        | [ e1; e2 ] -> (eval_int_expr e1, eval_int_expr e2)
        | _ -> raise Not_found
      in
      for i = i1 to i2 do
        let env = Templ.Env.add "integer" (Vint i) env in
        List.iter (print_ast env ()) al
      done
  | _ -> raise Not_found

let print_calendar conf _base =
  Util.html conf;
  let jd = eval_julian_day conf in
  let ifun =
    Templ.
      {
        eval_var = eval_var conf jd;
        eval_transl = (fun _ -> Templ.eval_transl conf);
        eval_predefined_apply = (fun _ -> raise Not_found);
        get_vother;
        set_vother;
        print_foreach = print_foreach conf jd;
      }
  in
  Templ.output conf ifun Templ.Env.empty () "calendar"

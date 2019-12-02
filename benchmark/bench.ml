open Geneweb

let style = ref Benchmark.Auto

let bench ?(t=1) name fn arg =
  if match Sys.getenv_opt "BENCH_FN" with None -> true | Some x -> x = name
  then Benchmark.throughput1 ~style:!style ~name t (List.map fn) arg
  else []

let list =
  [1;2;10;100;1000;10000;100000;1000000;10000000;100000000;1000000000]

let sosa_list =
  List.map Sosa.of_int list

let bench () =
  let suite =
    [ bench "Sosa.gen" (List.map Sosa.gen) [ sosa_list ]
    ; bench "Sosa.to_string_sep" (List.map @@ Sosa.to_string_sep ",") [ sosa_list ]
    ; bench "Sosa.to_string" (List.map Sosa.to_string) [ sosa_list ]
    ; bench "Sosa.of_string" (List.map Sosa.of_string) [ List.map string_of_int list ]
    ; bench "Sosa.branches" (List.map Sosa.branches) [ sosa_list ]
    ; bench "Place.normalize" Geneweb.Place.normalize
        [ "[foo-bar] - boobar (baz)" ; "[foo-bar] – boobar (baz)" ; "[foo-bar] — boobar (baz)" ]
    ; bench "Mutil.unsafe_tr" (fun s -> Mutil.unsafe_tr 'a' 'b' @@ "a" ^ s)
        [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
    ; bench "Mutil.tr" (fun s -> Mutil.tr 'a' 'b' @@ "a" ^ s)
        [ "aaaaaaaaaa" ; "bbbbbbbbbb" ; "abbbbbbbb" ; "bbbbbbbbba" ; "ababababab" ]
    ; bench "Mutil.contains" (Mutil.contains "foobarbaz")
        [ "foo" ; "bar" ; "baz" ; "foobarbaz!" ]
    ; bench "Place.compare_places" (Place.compare_places "[foo-bar] - baz, boobar")
        [ "[foo-bar] - baz, boobar"
        ; "[foo-bar] - baz, boobar, barboo"
        ; "baz, boobar"
        ]
    ; bench "Name.lower" Name.lower
        [ "étienne" ; "Étienne" ; "ÿvette" ; "Ÿvette" ; "Ĕtienne" ]
    ]
  in
  match Sys.getenv_opt "BENCH_BASE" with
  | Some bname when bname <> "" ->
    let conf =
      {Config.from="";manitou=false;supervisor=false;wizard=false;
       #ifdef API
         api_host="";api_port=0;
       #endif
         is_printed_by_template=false;friend=false;
       just_friend_wizard=false;user="";username="";
       auth_scheme=Config.NoAuth;pure_xhtml=false;command="";
       indep_command="";highlight="";lang="";default_lang="";
       default_sosa_ref=Gwdb.dummy_iper, None;multi_parents=false;
       can_send_image=false;authorized_wizards_notes=false;
       public_if_titles=false;public_if_no_date=false;
       cancel_links=false;setup_link=false;access_by_key=false;
       private_years=0;hide_names=false;use_restrict=false;
       no_image=false;no_note=false;bname="";cgi_passwd="";
       env=[];senv=[];henv=[];base_env=[];allowed_titles=lazy[];
       denied_titles=lazy[];xhs="";request=[];
       lexicon=Hashtbl.create 16;charset="";is_rtl=false;left="";
       right="";auth_file="";border=0;n_connect=None;
       today={Def.day=0;month=0;year=0;delta=0;prec=Def.Sure};
       today_wd=0;time=0,0,0;ctime=0.;image_prefix="";
       b_arg_for_basename=false}
    in
    let bench_w_base name fn args =
      let base = Gwdb.open_base bname in
      let r = bench name (fn base) args in
      Gwdb.close_base base ;
      r
    in
    bench_w_base
      "UpdateData.get_all_data"
      begin fun base conf -> UpdateData.get_all_data conf base end
      [ { conf with Config.env = ["data","place"] } ]
    ::
    bench_w_base
      "UpdateData.build_list_short"
      begin fun base conf ->
        UpdateData.build_list_short conf @@ UpdateData.build_list conf base
      end
      [ { conf with Config.env = ["data","src"] ; wizard = true } ]
    :: suite
  | _ -> suite

let () =
  let marshal = ref false in
  let tabulate = ref false in
  let file = ref "" in
  let name = ref "" in
  let speclist =
    [ ("--marshal", Arg.Set marshal, "")
    ; ("--tabulate", Arg.Set tabulate, "")
    ; ("--name", Arg.Set_string name, "")
    ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] [FILES]" in
  let anonfun s = file := s in
  Arg.parse speclist anonfun usage ;
  if !marshal then style := Benchmark.Nil ;
  if (!file <> "" && not !tabulate && not !marshal)
  || (!tabulate && !marshal)
  || (!marshal && !name = "")
  then begin Arg.usage speclist usage ; exit 2 end ;
  if !marshal then begin
    let samples : Benchmark.samples list = bench () in
    let ht =
      if Sys.file_exists !file then
        let ch = open_in_bin !file in
        let ht : (string, Benchmark.samples) Hashtbl.t = Marshal.from_channel ch in
        close_in ch ;
        ht
      else Hashtbl.create 256
    in
    let add k v = match Hashtbl.find_opt ht k with
      | Some v' -> Hashtbl.replace ht k (v :: v')
      | None -> Hashtbl.add ht k [ v ]
    in
    List.iter begin function
      | [ fn, t ] -> add fn (!name, t)
      | _ -> assert false
    end samples ;
    let ch = open_out_bin !file in
    Marshal.to_channel ch ht [] ;
    close_out ch
  end else if !tabulate then begin
    let ch = open_in_bin !file in
    let ht : (string, Benchmark.samples) Hashtbl.t = Marshal.from_channel ch in
    close_in ch ;
    Hashtbl.iter begin fun name samples ->
      Printf.printf "\n%s:\n" name ;
      Benchmark.tabulate samples
    end ht
  end else ignore @@ bench ()

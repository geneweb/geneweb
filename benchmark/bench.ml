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
    ; bench "Name.split_fname" Name.split_fname
        [ "Jean-Baptiste Emmanuel" ; "Jean Baptiste Emmanuel" ]
    ; bench "Name.split_sname" Name.split_sname
        [ "Jean-Baptiste Emmanuel" ; "Jean Baptiste Emmanuel" ]
    ]
  in
  match Sys.getenv_opt "BENCH_BASE" with
  | Some bname when bname <> "" ->
    let conf = Config.empty in
    let bench_w_base ?t name fn args =
      let base = Gwdb.open_base bname in
      let r = bench ?t name (fn base) args in
      Gwdb.close_base base ;
      r
    in
    bench_w_base
      "UpdateData.get_all_data"
      begin fun base conf -> UpdateData.get_all_data conf base end
      [ { conf with Config.env = ["data","place"] } ]
    ::
    bench_w_base
      "UpdateData.build_list"
      begin fun base conf -> UpdateData.build_list conf base end
      [ { conf with Config.env = ["data","src"] ; wizard = true }
      ; { conf with Config.env = ["data","place"] ; wizard = true } ]
    ::
    bench_w_base
      "UpdateData.build_list_short"
      begin fun base conf ->
        UpdateData.build_list_short conf @@ UpdateData.build_list conf base
      end
      [ { conf with Config.env = ["data","src"] ; wizard = true }
      ; { conf with Config.env = ["data","place"] ; wizard = true } ]
    ::
    bench_w_base "Check.check_base" ~t:10
      begin fun base _conf ->
        Check.check_base base
          (Sys.opaque_identity ignore)
          (Sys.opaque_identity ignore)
          (Sys.opaque_identity ignore)
      end
      [ conf ]
    ::
    bench_w_base "Perso.first_possible_duplication" ~t:10
      begin fun base _conf ->
        Gwdb.Collection.fold begin fun acc p ->
          (Perso.first_possible_duplication base (Gwdb.get_iper p) ([], [])) :: acc
        end [] (Gwdb.persons base)
      end
      [ conf ]
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

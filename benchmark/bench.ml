open Geneweb

let bench name n fn arg =
  ignore @@ Benchmark.latency1 ~name n fn arg

let () =
  bench "Sosa.gen" 1000000L
    (List.map Sosa.gen)
    (List.map Sosa.of_int [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15])

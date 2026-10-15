let check ?pos testable ?(msg = "") expected actual =
  match pos with
  | Some pos -> Alcotest.check' ~pos testable ~msg ~expected ~actual
  | None -> Alcotest.check testable msg expected actual

let string ?pos ?msg expected actual =
  check ?pos Alcotest.string ?msg expected actual

let int ?pos ?msg expected actual = check ?pos Alcotest.int ?msg expected actual

let bool ?pos ?msg expected actual =
  check ?pos Alcotest.bool ?msg expected actual

let list testable ?pos ?msg expected actual =
  check ?pos (Alcotest.list testable) ?msg expected actual

let option testable ?pos ?msg expected actual =
  check ?pos (Alcotest.option testable) ?msg expected actual

let pair t1 t2 ?pos ?msg expected actual =
  check ?pos (Alcotest.pair t1 t2) ?msg expected actual

let string_pair ?pos ?msg expected actual =
  pair Alcotest.string Alcotest.string ?pos ?msg expected actual

let check_raises ?pos msg exn f =
  match pos with
  | Some pos -> Alcotest.check_raises ~pos msg exn f
  | None -> Alcotest.check_raises msg exn f

let fail ?pos msg =
  match pos with
  | Some pos -> Alcotest.fail ~pos msg
  | None -> Alcotest.fail msg

let case name f = Alcotest.test_case name `Quick f
let suite name cases = (name, cases)

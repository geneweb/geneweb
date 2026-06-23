module A = Alcotest
module Q = QCheck

let dec = Gwd_lib.Base64.decode

let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let encode s =
  let len = String.length s in
  let buf = Buffer.create ((len + 2) / 3 * 4) in
  let byte i = if i < len then Char.code s.[i] else 0 in
  let rec loop i =
    if i < len then begin
      let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let rem = len - i in
      Buffer.add_char buf alphabet.[(n lsr 18) land 0x3F];
      Buffer.add_char buf alphabet.[(n lsr 12) land 0x3F];
      Buffer.add_char buf
        (if rem >= 2 then alphabet.[(n lsr 6) land 0x3F] else '=');
      Buffer.add_char buf (if rem >= 3 then alphabet.[n land 0x3F] else '=');
      loop (i + 3)
    end
  in
  loop 0;
  Buffer.contents buf

let test_vectors () =
  let check expected input = A.(check string) input expected (dec input) in
  check "" "";
  check "f" "Zg==";
  check "fo" "Zm8=";
  check "foo" "Zm9v";
  check "foob" "Zm9vYg==";
  check "fooba" "Zm9vYmE=";
  check "foobar" "Zm9vYmFy";
  check "Aladdin:open sesame" "QWxhZGRpbjpvcGVuIHNlc2FtZQ=="

let test_malformed () =
  let empty input = A.(check string) input "" (dec input) in
  empty "Z";
  empty "Zg";
  empty "Zm9"

let byte = Q.Gen.(map Char.chr (int_bound 255))

let roundtrip =
  let gen = Q.Gen.string_size ~gen:byte (Q.Gen.int_bound 64) in
  let t =
    Q.Test.make ~count:1000 ~name:"decode (encode s) = s"
      (Q.make ~print:String.escaped gen)
    @@ fun s -> String.equal (dec (encode s)) s
  in
  QCheck_alcotest.to_alcotest t

let () =
  A.run __FILE__
    [
      ( "Base64.decode",
        [
          A.test_case "RFC 4648 vectors" `Quick test_vectors;
          A.test_case "malformed length" `Quick test_malformed;
          roundtrip;
        ] );
    ]

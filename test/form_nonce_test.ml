open Alcotest
open Geneweb

let bfile = "form_nonce_test.gwb"

let env nonce occu =
  [
    ("m", Adef.encoded "MOD_IND_OK");
    ("nonce", Adef.encoded nonce);
    ("occu", Adef.encoded occu);
  ]

let test_nonce () =
  (try Sys.remove (Filename.remove_extension bfile ^ ".nonces")
   with Sys_error _ -> ());
  let n = FormNonce.fresh () in
  (check int) "length" 32 (String.length n);
  (check bool) "fresh" true (n <> FormNonce.fresh ());
  let fp = FormNonce.fingerprint (env n "meunier") in
  (check string) "nonce not in the fingerprint" fp
    (FormNonce.fingerprint (env "other" "meunier"));
  (check bool) "not saved" false
    (FormNonce.saved bfile ~nonce:n ~fingerprint:fp);
  FormNonce.record bfile ~nonce:n ~fingerprint:fp;
  (check bool) "saved" true (FormNonce.saved bfile ~nonce:n ~fingerprint:fp);
  (check bool) "same form, other entry" false
    (FormNonce.saved bfile ~nonce:n
       ~fingerprint:(FormNonce.fingerprint (env n "tisserand")));
  (check bool) "other form" false
    (FormNonce.saved bfile ~nonce:(FormNonce.fresh ()) ~fingerprint:fp)

let v = [ ("form-nonce", [ test_case "nonce" `Quick test_nonce ]) ]

let test_meth_conversion () =
  let check_meth expected str =
    let actual = Http.meth_of_string str in
    Geneweb_test_utils.Check.string ~pos:__POS__ ~msg:"Http.string_of_meth"
      (Http.string_of_meth expected)
      (Http.string_of_meth actual)
  in
  check_meth Http.GET "GET";
  check_meth Http.POST "POST";
  check_meth Http.HEAD "HEAD";
  check_meth Http.PUT "PUT";
  check_meth Http.DELETE "DELETE";
  check_meth (Http.OTHER "CUSTOM") "CUSTOM";
  check_meth Http.GET "get";
  check_meth Http.POST "post";
  check_meth Http.POST "Post"

let test_parse_request_line_get () =
  let req = [ "GET /base?m=NG&b=mybase HTTP/1.1"; "Host: localhost:2317" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse valid GET"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line method" "GET"
        (Http.string_of_meth parsed.meth);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "base"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query" (Some "m=NG&b=mybase")
        (Option.map Http.string_of_query parsed.query);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line version" (Some "HTTP/1.1")
        (Option.map Http.string_of_http_version parsed.version)

let test_parse_request_line_post () =
  let req = [ "POST /base HTTP/1.0"; "Content-Length: 4" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse valid POST"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line method" "POST"
        (Http.string_of_meth parsed.meth);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "base"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query" None
        (Option.map Http.string_of_query parsed.query);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line version" (Some "HTTP/1.0")
        (Option.map Http.string_of_http_version parsed.version)

let test_parse_request_line_crlf () =
  let req = [ "GET /base HTTP/1.1\r" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse CRLF terminated line"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line method" "GET"
        (Http.string_of_meth parsed.meth);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "base"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line version" (Some "HTTP/1.1")
        (Option.map Http.string_of_http_version parsed.version)

let test_parse_request_line_empty_query () =
  let req = [ "POST /base? HTTP/1.1" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed on trailing question mark"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "base"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query should be None" None
        (Option.map Http.string_of_query parsed.query)

let test_parse_request_line_wizard_mode () =
  let req =
    [ "GET /base_w?m=SOME_MODE&param=value HTTP/1.1"; "Host: localhost:2317" ]
  in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse wizard mode GET"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path retains _w suffix" "base_w"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query preserves mode with underscore"
        (Some "m=SOME_MODE&param=value")
        (Option.map Http.string_of_query parsed.query)

let test_parse_request_line_post_long_query () =
  let long_q = "m=UPDATE&" ^ String.make 1000 'a' ^ "=b" in
  let req = [ "POST /base_w?" ^ long_q ^ " HTTP/1.1"; "Content-Length: 4" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse POST with long query"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path retains _w suffix" "base_w"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query preserves long strings"
        (Some long_q)
        (Option.map Http.string_of_query parsed.query)

let test_parse_request_line_asterisk () =
  let req = [ "OPTIONS * HTTP/1.1" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse asterisk target"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line method" "OPTIONS"
        (Http.string_of_meth parsed.meth);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "*"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query should be None" None
        (Option.map Http.string_of_query parsed.query);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line version" (Some "HTTP/1.1")
        (Option.map Http.string_of_http_version parsed.version)

let test_parse_request_line_no_version () =
  let req = [ "GET /base?m=NG" ] in
  match Http.parse_request_line req with
  | None ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_request_line failed to parse request without version"
  | Some parsed ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line method" "GET"
        (Http.string_of_meth parsed.meth);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_request_line path" "base"
        (Http.string_of_path parsed.path);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line query" (Some "m=NG")
        (Option.map Http.string_of_query parsed.query);
      Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
        ~msg:"Http.parse_request_line version is None" None
        (Option.map Http.string_of_http_version parsed.version)

let test_parse_request_line_invalid () =
  let check_none msg req =
    match Http.parse_request_line req with
    | None -> ()
    | Some _ ->
        Geneweb_test_utils.Check.fail ~pos:__POS__ (msg ^ " expected None")
  in
  check_none "Http.parse_request_line empty" [];
  check_none "Http.parse_request_line malformed" [ "INVALID" ];
  check_none "Http.parse_request_line header line rejected"
    [ "Host: localhost:2317" ];
  check_none "Http.parse_request_line target without slash rejected"
    [ "GET base HTTP/1.1" ]

let test_header_get () =
  let req =
    [
      "GET /base HTTP/1.1";
      "Host: localhost:2317";
      "Content-Type: application/x-www-form-urlencoded";
      "User-Agent: Mozilla/5.0";
    ]
  in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.Header.get content-type trimmed"
    (Some "application/x-www-form-urlencoded")
    (Http.Header.get ~request:req "content-type");
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.Header.get case-insensitive" (Some "localhost:2317")
    (Http.Header.get ~request:req "HOST");
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.Header.get missing header" None
    (Http.Header.get ~request:req "Authorization")

let test_parse_content_type () =
  let ct_multipart =
    "multipart/form-data; boundary=----WebKitFormBoundaryXYZ"
  in
  (match Http.parse_content_type ct_multipart with
  | Http.MultipartForm { boundary } ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_content_type boundary" "----WebKitFormBoundaryXYZ"
        (Http.Boundary.to_string boundary)
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse multipart");
  let ct_multipart_quoted =
    "multipart/form-data; boundary=\"----WebKitFormBoundaryQuoted\""
  in
  (match Http.parse_content_type ct_multipart_quoted with
  | Http.MultipartForm { boundary } ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_content_type quoted boundary"
        "----WebKitFormBoundaryQuoted"
        (Http.Boundary.to_string boundary)
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse quoted boundary");
  let ct_multipart_upper = "MULTIPART/FORM-DATA; boundary=BOUNDARY_UPPER" in
  (match Http.parse_content_type ct_multipart_upper with
  | Http.MultipartForm { boundary } ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_content_type uppercase boundary" "BOUNDARY_UPPER"
        (Http.Boundary.to_string boundary)
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse uppercase multipart");
  let ct_multipart_attr_upper = "multipart/form-data; BOUNDARY=foo" in
  (match Http.parse_content_type ct_multipart_attr_upper with
  | Http.MultipartForm { boundary } ->
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.parse_content_type uppercase boundary attribute" "foo"
        (Http.Boundary.to_string boundary)
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse uppercase boundary attribute");
  let ct_multipart_no_boundary = "multipart/form-data" in
  (match Http.parse_content_type ct_multipart_no_boundary with
  | Http.Other "multipart/form-data" -> ()
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type should fallback to Other when boundary is \
         missing");
  let ct_urlencoded = "application/x-www-form-urlencoded; charset=UTF-8" in
  (match Http.parse_content_type ct_urlencoded with
  | Http.UrlEncoded -> ()
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse urlencoded");
  let ct_urlencoded_with_spaces =
    "  application/x-www-form-urlencoded; charset=UTF-8  "
  in
  (match Http.parse_content_type ct_urlencoded_with_spaces with
  | Http.UrlEncoded -> ()
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse urlencoded with leading spaces");
  let ct_urlencoded_upper = "APPLICATION/X-WWW-FORM-URLENCODED" in
  (match Http.parse_content_type ct_urlencoded_upper with
  | Http.UrlEncoded -> ()
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse uppercase urlencoded");
  let ct_json = "application/json" in
  match Http.parse_content_type ct_json with
  | Http.Other "application/json" -> ()
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse other"

let test_parse_query_and_merge () =
  let url_params =
    Http.Env.to_list
      (Http.parse_query_params (Http.query_of_string "m=NG&b=base"))
  in
  let body_params =
    Http.Env.to_list
      (Http.parse_query_params (Http.query_of_string "foo=bar&m=OVERRIDDEN"))
  in
  let merged = Http.merge_env url_params body_params in
  let get_val k env = (List.assoc k env : Adef.encoded_string :> string) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.merge_env URL param takes precedence" "NG" (get_val "m" merged);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.merge_env URL param retained" "base" (get_val "b" merged);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.merge_env body param retained" "bar" (get_val "foo" merged);
  let m_count = List.length (List.filter (fun (k, _) -> k = "m") merged) in
  Geneweb_test_utils.Check.int ~pos:__POS__
    ~msg:"Http.merge_env deduplicates shadowed keys" 1 m_count

let test_extract_multipart () =
  let boundary = Http.Boundary.of_string "AaB03x" in
  let payload =
    "--AaB03x\r\n\
     content-disposition: form-data; name=\"field1\"\r\n\
     \r\n\
     value1\r\n\
     --AaB03x\r\n\
     content-disposition: form-data; name=\"field2\"\r\n\
     \r\n\
     value2\r\n\
     --AaB03x--\r\n"
  in
  let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
  let get_val k = Option.value ~default:"" (Http.Env.get env k) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart field1" "value1" (get_val "field1");
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart field2" "value2" (get_val "field2")

let test_extract_multipart_file () =
  let boundary = Http.Boundary.of_string "boundary123" in
  let payload =
    "--boundary123\r\n\
     content-disposition: form-data; name=\"upload\"; filename=\"test.ged\"\r\n\
     \r\n\
     0 HEAD\r\n\
     1 SOUR GeneWeb\r\n\
     0 TRLR\r\n\
     --boundary123--\r\n"
  in
  let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
  let get_val k = Option.value ~default:"" (Http.Env.get env k) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart file name" "test.ged" (get_val "upload_name");
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart file content"
    "0 HEAD\r\n1 SOUR GeneWeb\r\n0 TRLR\r\n" (get_val "upload")

let test_extract_multipart_quoted_boundary () =
  let ct = "multipart/form-data; boundary=\"my_boundary\"" in
  match Http.parse_content_type ct with
  | Http.MultipartForm { boundary } ->
      let payload =
        "--my_boundary\r\n\
         content-disposition: form-data; name=\"field\"\r\n\
         \r\n\
         val\r\n\
         --my_boundary--\r\n"
      in
      let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
      let get_val k = Option.value ~default:"" (Http.Env.get env k) in
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Http.extract_multipart with quoted boundary" "val"
        (get_val "field")
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.parse_content_type failed to parse quoted boundary"

let test_extract_boundary_quotes () =
  let ct = Adef.encoded "multipart/form-data; boundary=\"xyz\"" in
  let boundary = Http.extract_boundary ct in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_boundary strips quotes" "xyz"
    (Http.Boundary.to_string boundary)

let test_extract_boundary_case_insensitive () =
  let ct_upper = Adef.encoded "multipart/form-data; BOUNDARY=xyz" in
  let boundary_upper = Http.extract_boundary ct_upper in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_boundary uppercase attribute" "xyz"
    (Http.Boundary.to_string boundary_upper);
  let ct_mixed = Adef.encoded "multipart/form-data; Boundary=abc" in
  let boundary_mixed = Http.extract_boundary ct_mixed in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_boundary mixed-case attribute" "abc"
    (Http.Boundary.to_string boundary_mixed)

let test_extract_boundary_opt () =
  let ct_multipart = Adef.encoded "multipart/form-data; boundary=xyz123" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt multipart" (Some "xyz123")
    (Option.map Http.Boundary.to_string
       (Http.extract_boundary_opt ct_multipart));
  let ct_quoted =
    Adef.encoded "multipart/form-data; boundary=\"quoted_boundary\""
  in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt quoted boundary" (Some "quoted_boundary")
    (Option.map Http.Boundary.to_string (Http.extract_boundary_opt ct_quoted));
  let ct_case = Adef.encoded "multipart/form-data; BOUNDARY=xyz" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt case-insensitive" (Some "xyz")
    (Option.map Http.Boundary.to_string (Http.extract_boundary_opt ct_case));
  let ct_urlencoded = Adef.encoded "application/x-www-form-urlencoded" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt urlencoded" None
    (Option.map Http.Boundary.to_string
       (Http.extract_boundary_opt ct_urlencoded));
  let ct_other = Adef.encoded "text/plain" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt text/plain" None
    (Option.map Http.Boundary.to_string (Http.extract_boundary_opt ct_other));
  let ct_no_boundary = Adef.encoded "multipart/form-data" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt multipart without boundary" None
    (Option.map Http.Boundary.to_string
       (Http.extract_boundary_opt ct_no_boundary));
  let ct_empty = Adef.encoded "" in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Http.extract_boundary_opt empty" None
    (Option.map Http.Boundary.to_string (Http.extract_boundary_opt ct_empty))

let test_extract_boundary_backward_compat () =
  let ct_multipart = Adef.encoded "multipart/form-data; boundary=xyz123" in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_boundary backward compat success" "xyz123"
    (Http.Boundary.to_string (Http.extract_boundary ct_multipart));
  let ct_non_multipart = Adef.encoded "application/x-www-form-urlencoded" in
  match Http.extract_boundary ct_non_multipart with
  | _ ->
      Geneweb_test_utils.Check.fail ~pos:__POS__
        "Http.extract_boundary expected Not_found on non-multipart"
  | exception Not_found -> ()

let test_extract_multipart_empty_field () =
  let boundary = Http.Boundary.of_string "boundary_empty" in
  let payload =
    "--boundary_empty\r\n\
     content-disposition: form-data; name=\"notes\"\r\n\
     \r\n\
     --boundary_empty\r\n\
     content-disposition: form-data; name=\"name\"\r\n\
     \r\n\
     John\r\n\
     --boundary_empty--\r\n"
  in
  let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
  let get_val k = Option.value ~default:"" (Http.Env.get env k) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart empty field" "" (get_val "notes");
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart subsequent field" "John" (get_val "name")

let test_extract_multipart_multi_headers () =
  let boundary = Http.Boundary.of_string "boundary_multi" in
  let payload =
    "--boundary_multi\r\n\
     content-disposition: form-data; name=\"notes\"\r\n\
     content-type: text/plain; charset=UTF-8\r\n\
     \r\n\
     My multi-header note\r\n\
     --boundary_multi--\r\n"
  in
  let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
  let get_val k = Option.value ~default:"" (Http.Env.get env k) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart multi-header field" "My multi-header note"
    (get_val "notes")

let test_extract_multipart_file_with_content_type () =
  let boundary = Http.Boundary.of_string "boundary_file_ct" in
  let payload =
    "--boundary_file_ct\r\n\
     content-disposition: form-data; name=\"upload\"; filename=\"test.ged\"\r\n\
     content-type: text/plain\r\n\
     \r\n\
     0 HEAD\r\n\
     1 SOUR GeneWeb\r\n\
     0 TRLR\r\n\
     --boundary_file_ct--\r\n"
  in
  let _, env = Http.extract_multipart boundary (Adef.encoded payload) in
  let get_val k = Option.value ~default:"" (Http.Env.get env k) in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart file name with content-type" "test.ged"
    (get_val "upload_name");
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Http.extract_multipart file content excludes content-type header"
    "0 HEAD\r\n1 SOUR GeneWeb\r\n0 TRLR\r\n" (get_val "upload")

let v =
  [
    ( "http-request-methods",
      [ Alcotest.test_case "method conversions" `Quick test_meth_conversion ] );
    ( "http-headers",
      [ Alcotest.test_case "header extraction" `Quick test_header_get ] );
    ( "http-request-line",
      [
        Alcotest.test_case "parse GET" `Quick test_parse_request_line_get;
        Alcotest.test_case "parse POST" `Quick test_parse_request_line_post;
        Alcotest.test_case "parse CRLF line" `Quick test_parse_request_line_crlf;
        Alcotest.test_case "parse empty query" `Quick
          test_parse_request_line_empty_query;
        Alcotest.test_case "parse wizard mode" `Quick
          test_parse_request_line_wizard_mode;
        Alcotest.test_case "parse long POST query" `Quick
          test_parse_request_line_post_long_query;
        Alcotest.test_case "parse asterisk target" `Quick
          test_parse_request_line_asterisk;
        Alcotest.test_case "parse HTTP/0.9 without version" `Quick
          test_parse_request_line_no_version;
        Alcotest.test_case "parse invalid" `Quick
          test_parse_request_line_invalid;
      ] );
    ( "http-content-type",
      [
        Alcotest.test_case "parse content types" `Quick test_parse_content_type;
      ] );
    ( "http-env-merging",
      [
        Alcotest.test_case "parse query and merge env" `Quick
          test_parse_query_and_merge;
      ] );
    ( "http-multipart",
      [
        Alcotest.test_case "extract multipart fields" `Quick
          test_extract_multipart;
        Alcotest.test_case "extract multipart file" `Quick
          test_extract_multipart_file;
        Alcotest.test_case "extract multipart quoted boundary" `Quick
          test_extract_multipart_quoted_boundary;
        Alcotest.test_case "extract boundary with quotes" `Quick
          test_extract_boundary_quotes;
        Alcotest.test_case "extract boundary case-insensitive" `Quick
          test_extract_boundary_case_insensitive;
        Alcotest.test_case "extract boundary opt" `Quick
          test_extract_boundary_opt;
        Alcotest.test_case "extract boundary backward compat" `Quick
          test_extract_boundary_backward_compat;
        Alcotest.test_case "extract multipart empty field" `Quick
          test_extract_multipart_empty_field;
        Alcotest.test_case "extract multipart multi-headers" `Quick
          test_extract_multipart_multi_headers;
        Alcotest.test_case "extract multipart file with content-type" `Quick
          test_extract_multipart_file_with_content_type;
      ] );
  ]

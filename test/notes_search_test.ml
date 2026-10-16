open Geneweb

let note =
  {|= Histoire =
Le ''moulin'' de <b>Saint-Pair</b> appartenait à [[Jean/Dupont/0/Jean Dupont]],
puis à [[[aliases/la famille Martin]]]. Voir <a href="x">le cadastre</a> &amp; les
registres du Château.|}

let plain = NotesSearch.plain_text note

let test_plain_text () =
  Alcotest.(check string)
    "plain text"
    "Histoire Le moulin de Saint-Pair appartenait à Jean Dupont, puis à la \
     famille Martin. Voir le cadastre & les registres du Château."
    plain

let test_count () =
  let count cs s = NotesSearch.count_occurrences cs [ s ] plain in
  Alcotest.(check int) "case-insensitive" 1 (count false "MOULIN");
  Alcotest.(check int) "case-sensitive" 0 (count true "MOULIN");
  Alcotest.(check int) "link text" 1 (count false "famille");
  Alcotest.(check int) "accent-insensitive" 1 (count false "chateau");
  Alcotest.(check int) "markup not matched" 0 (count false "href");
  Alcotest.(check int) "decoded entity" 1 (count false "& les");
  Alcotest.(check string)
    "named and numeric entities" "… — «x» &"
    (NotesSearch.plain_text "&hellip; &mdash; &laquo;x&raquo; &#x26;")

let test_find_in_text () =
  Alcotest.(check (option (pair int int)))
    "empty pattern" None
    (Util.find_in_text false "" "abc" 0);
  Alcotest.(check (option (pair int int)))
    "from offset" (Some (4, 7))
    (Util.find_in_text false "abc" "abc abc" 1)

let test_snippet () =
  let snip = NotesSearch.snippet ~context:20 false [ "Dupont" ] plain in
  let hl = {|<span class="found">Dupont</span>|} in
  let n = String.length hl in
  let rec contains i =
    i + n <= String.length snip && (String.sub snip i n = hl || contains (i + 1))
  in
  Alcotest.(check bool) "highlighted" true (contains 0);
  Alcotest.(check string)
    "inline tags keep words whole" "moulin"
    (NotesSearch.plain_text "mou<i>lin</i>");
  Alcotest.(check string)
    "block tags separate words" "a b"
    (NotesSearch.plain_text "a<br>b");
  Alcotest.(check bool)
    "truncated before" true
    (String.starts_with ~prefix:"… " snip);
  Alcotest.(check string)
    "escaped highlight" {|a &#60;<span class="found">b</span>&#62;|}
    (NotesSearch.highlight true [ "b" ] "a <b>");
  Alcotest.(check string) "empty text" "" (NotesSearch.snippet false [ "x" ] "")

let test_highlight_html () =
  let hl t = NotesSearch.highlight_html false [ t ] in
  Alcotest.(check string)
    "first occurrence gets the anchor"
    {|<p><span class="found" id="first-found">Moulin</span> et <b><span class="found">moulin</span></b></p>|}
    (hl "moulin" "<p>Moulin et <b>moulin</b></p>");
  Alcotest.(check string)
    "tags and attributes untouched"
    {|<a href="moulin.html" title="moulin">le <span class="found" id="first-found">moulin</span></a>|}
    (hl "moulin" {|<a href="moulin.html" title="moulin">le moulin</a>|});
  Alcotest.(check string)
    "script and style untouched"
    {|<script>var moulin = "<b>moulin</b>";</script><style>.moulin{}</style>x|}
    (hl "moulin"
       {|<script>var moulin = "<b>moulin</b>";</script><style>.moulin{}</style>x|});
  Alcotest.(check string)
    "comments untouched" "<!-- moulin -->"
    (hl "moulin" "<!-- moulin -->");
  Alcotest.(check string)
    "entity matched as its character, wrapped whole"
    {|cadastre <span class="found" id="first-found">&#38; les</span>|}
    (hl "& les" "cadastre &#38; les");
  Alcotest.(check string)
    "entity name not matched" "&amp;"
    (hl "amp" "&amp;");
  Alcotest.(check string)
    "white space run matches one space"
    "<span class=\"found\" id=\"first-found\">Jean\n  Dupont</span>"
    (hl "jean dupont" "Jean\n  Dupont");
  Alcotest.(check string)
    "accent-insensitive" {|<span class="found" id="first-found">Château</span>|}
    (hl "chateau" "Château");
  Alcotest.(check string) "no match" "<p>rien</p>" (hl "moulin" "<p>rien</p>")

let test_terms () =
  Alcotest.(check (list string))
    "words and quoted phrase" [ "Annie"; "de Villoutreys"; "album" ]
    (NotesSearch.terms false {|  Annie "de Villoutreys" album |});
  Alcotest.(check (list string))
    "duplicates dropped" [ "henri"; "moulin" ]
    (NotesSearch.terms false "henri moulin Henri");
  Alcotest.(check (list string))
    "case-sensitive keeps both" [ "henri"; "Henri" ]
    (NotesSearch.terms true "henri Henri");
  Alcotest.(check (list string)) "only quotes" [] (NotesSearch.terms false {|""|});
  Alcotest.(check (list string))
    "unclosed quote" [ "a"; "b c" ]
    (NotesSearch.terms false {|a "b c|})

let test_exclusion () =
  let q = NotesSearch.query false in
  Alcotest.(check (pair (list string) (list string)))
    "word excluded" ([ "aaa" ], [ "bbb" ])
    (q "aaa -bbb");
  Alcotest.(check (pair (list string) (list string)))
    "phrase excluded" ([ "Annie" ], [ "de Villoutreys" ])
    (q {|Annie -"de Villoutreys"|});
  Alcotest.(check (pair (list string) (list string)))
    "hyphen inside a word" ([ "Saint-Pair" ], [])
    (q "Saint-Pair");
  Alcotest.(check (pair (list string) (list string)))
    "lone hyphen ignored" ([ "a"; "b" ], [])
    (q "a - b");
  Alcotest.(check (pair (list string) (list string)))
    "only exclusions" ([], [ "x" ])
    (q "-x");
  Alcotest.(check (list string))
    "excluded terms not highlighted" [ "aaa" ]
    (NotesSearch.terms false "aaa -bbb")

let test_and () =
  Alcotest.(check bool)
    "all present" true
    (NotesSearch.mem_all false [ "moulin"; "dupont" ] plain);
  Alcotest.(check bool)
    "one missing" false
    (NotesSearch.mem_all false [ "moulin"; "durand" ] plain);
  Alcotest.(check int)
    "occurrences of all terms" 2
    (NotesSearch.count_occurrences false [ "moulin"; "dupont" ] plain);
  Alcotest.(check string)
    "several terms highlighted"
    {|<span class="found" id="first-found">Henri</span> et <span class="found">Annie</span>|}
    (NotesSearch.highlight_html false [ "annie"; "henri" ] "Henri et Annie");
  Alcotest.(check string)
    "longest term wins at the same position"
    {|<span class="found" id="first-found">Henriette</span>|}
    (NotesSearch.highlight_html false [ "henri"; "henriette" ] "Henriette")

let v =
  [
    ( "notes-search",
      [
        Alcotest.test_case "plain_text" `Quick test_plain_text;
        Alcotest.test_case "count_occurrences" `Quick test_count;
        Alcotest.test_case "find_in_text" `Quick test_find_in_text;
        Alcotest.test_case "snippet" `Quick test_snippet;
        Alcotest.test_case "highlight_html" `Quick test_highlight_html;
        Alcotest.test_case "terms" `Quick test_terms;
        Alcotest.test_case "and" `Quick test_and;
        Alcotest.test_case "exclusion" `Quick test_exclusion;
      ] );
  ]

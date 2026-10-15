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
  let count cs s = NotesSearch.count_occurrences cs s plain in
  Alcotest.(check int) "case-insensitive" 1 (count false "MOULIN");
  Alcotest.(check int) "case-sensitive" 0 (count true "MOULIN");
  Alcotest.(check int) "link text" 1 (count false "famille");
  Alcotest.(check int) "accent-insensitive" 1 (count false "chateau");
  Alcotest.(check int) "markup not matched" 0 (count false "href");
  Alcotest.(check int) "decoded entity" 1 (count false "& les")

let test_find_in_text () =
  Alcotest.(check (option (pair int int)))
    "empty pattern" None
    (Util.find_in_text false "" "abc" 0);
  Alcotest.(check (option (pair int int)))
    "from offset" (Some (4, 7))
    (Util.find_in_text false "abc" "abc abc" 1)

let test_snippet () =
  let snip = NotesSearch.snippet ~context:20 false "Dupont" plain in
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
    (NotesSearch.highlight true "b" "a <b>");
  Alcotest.(check string) "empty text" "" (NotesSearch.snippet false "x" "")

let v =
  [
    ( "notes-search",
      [
        Alcotest.test_case "plain_text" `Quick test_plain_text;
        Alcotest.test_case "count_occurrences" `Quick test_count;
        Alcotest.test_case "find_in_text" `Quick test_find_in_text;
        Alcotest.test_case "snippet" `Quick test_snippet;
      ] );
  ]

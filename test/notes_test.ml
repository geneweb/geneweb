let test_insert_brs ~expected ~input =
  (Alcotest.check Alcotest.string)
    "same_string" expected
    (Geneweb.Notes.insert_brs
       (Geneweb.Util.string_with_macros ~conf:Geneweb.Config.empty ~env:[] input))

let s1 = {|A note with
a newline|}

let s1' = {|A note with<br>
a newline|}

let s2 = {|


there are many newlines before the actual text
|}

let s2' = {|<br>
<br>
<br>
there are many newlines before the actual text
|}

let s3 = {|*puce1
*puce2

*puce3
some text after the wiki
|}

let s3' = {|*puce1
*puce2

*puce3
some text after the wiki
|}

let s4 =
  {|<br>
sometimes we already have brs in the note<br> and <br>
we need to avoid inserting new ones<br>
<br>
only insert
when
appropriate|}

let s4' =
  {|<br>
sometimes we already have brs in the note<br> and <br>
we need to avoid inserting new ones<br>
<br>
only insert<br>
when<br>
appropriate|}

let s5 =
  {|we can write text
in paragraphs

<br> is here

multiple times and multiple forms
<br/ ><br/ >
<br><br/>
<BR/  ><BR    />

some html <div>
everything
is
fine
</div>
|}

let s5' =
  {|we can write text<br>
in paragraphs

<br> is here

multiple times and multiple forms
<br><br>
<br><br>
<br><br>
<br>
some html <div>
everything
is
fine
</div>
|}

let s6 = {|<br>

coucou|}

let s6' = {|<br>
<br>
coucou|}

let s7 = {|<a title="&gt;">&gt;</a>|}
let s7' = {|<a title=">">&gt;</a>|}
let s8 = {|https://www.some_site.org?something=value&other_thing=value&pre=42|}

let s8' =
  {|<a href="https://www.some_site.org?something=value&amp;other_thing=value&amp;pre=42" target="_blank">https://www.some_site.org?something=value&amp;other_thing=value&amp;pre=42</a>|}

let test_insert_brs () =
  let cases =
    [
      (s1, s1');
      (s2, s2');
      (s3, s3');
      (s4, s4');
      (s5, s5');
      (s6, s6');
      (s7, s7');
      (s8, s8');
    ]
  in
  List.iter (fun (input, expected) -> test_insert_brs ~input ~expected) cases

let v =
  [
    ( "br-insertion-in-notes",
      [ Alcotest.test_case "insert brs" `Quick test_insert_brs ] );
  ]

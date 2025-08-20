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

let s9 = {|<i>youpla
boom</i>|}

let s9' = {|<i>youpla<br>
boom</i>|}

let s10 = {|<b>youpla
boom</b>|}

let s10' = {|<b>youpla<br>
boom</b>|}

let s11 = {|<em>youpla
boom</em>|}

let s11' = {|<em>youpla<br>
boom</em>|}

let s12 = {|<u>youpla
boom</u>|}

let s12' = {|<u>youpla<br>
boom</u>|}

let s13 = {|<s>youpla
boom</s>|}

let s13' = {|<s>youpla<br>
boom</s>|}

let s14 = {|<strong>youpla
boom</strong>|}

let s14' = {|<strong>youpla<br>
boom</strong>|}

let s15 = {|<strike>youpla
boom</strike>|}

let s15' = {|<strike>youpla<br>
boom</strike>|}

let s16 = {|<i> some text <p>
some
other
text
</p> yet some other text </i>|}

let s17 =
  {|<i> some
text <p>
some
<b>other
text</b>
</p>
yet some
other text
</i>|}

let s17' =
  {|<i> some<br>
text <p>
some
<b>other<br>
text</b>
</p><br>
yet some<br>
other text
</i>|}

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
      (s9, s9');
      (s10, s10');
      (s11, s11');
      (s12, s12');
      (s13, s13');
      (s14, s14');
      (s15, s15');
      (s16, s16);
      (s17, s17');
    ]
  in
  List.iter (fun (input, expected) -> test_insert_brs ~input ~expected) cases

let v =
  [
    ( "br-insertion-in-notes",
      [ Alcotest.test_case "insert brs" `Quick test_insert_brs ] );
  ]

(* lexwrap.ml — rewrap a GeneWeb lexicon file so that every physical line
   is shorter than a given width (default 80), using '\' end-of-line
   continuations. Existing continuations in the input are joined first,
   then each logical line is re-wrapped at word boundaries.

   Build:  ocamlopt lexwrap.ml -o lexwrap   (stdlib only)
   Usage:  lexwrap <lexicon.txt> [-o <output>] [-w <width>]
*)

let width = ref 80
let out_file = ref ""
let in_file = ref ""

(* indentation of continuation lines; 2 spaces so they can never be
   mistaken for a key line (keys are indented with exactly 4 spaces) *)
let cont_indent = "  "
let usage = "usage: lexwrap <lexicon.txt> [-o <output>] [-w <width>]"

let speclist =
  [
    ("-o", Arg.Set_string out_file, "<file>  output file (default stdout)");
    ("-w", Arg.Set_int width, "<n>     lines must be < n chars (default 80)");
  ]

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

let ltrim s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do
    incr i
  done;
  String.sub s !i (n - !i)

(* Read one logical line: a physical line ending with '\' continues on the
   next one; segments are joined with a single space (same convention as
   input_lexicon). *)
let read_logical ic =
  let buf = Buffer.create 120 in
  let rec loop first =
    let line = input_line ic in
    let line = if first then line else String.trim line in
    let len = String.length line in
    if len > 0 && line.[len - 1] = '\\' then begin
      Buffer.add_string buf (rtrim (String.sub line 0 (len - 1)));
      Buffer.add_char buf ' ';
      loop false
    end
    else Buffer.add_string buf line
  in
  loop true;
  Buffer.contents buf

(* Wrap a logical line into physical lines of length < !width.
   The line's own leading spaces (e.g. the 4-space key indent) stay on the
   first physical line; continuation lines get cont_indent. All physical
   lines except the last end with " \\". *)
let wrap s =
  let w = !width - 1 in
  let n = String.length s in
  let ind = ref 0 in
  while !ind < n && s.[!ind] = ' ' do
    incr ind
  done;
  let first_pre = String.sub s 0 !ind in
  let body = String.sub s !ind (n - !ind) in
  let rec loop pre cur acc =
    if String.length pre + String.length cur <= w then
      List.rev ((pre ^ cur) :: acc)
    else
      let budget = w - 2 - String.length pre in
      let cut =
        if budget < 1 then None
        else begin
          (* last space at or before budget *)
          let i = ref (min budget (String.length cur - 1)) in
          while !i > 0 && cur.[!i] <> ' ' do
            decr i
          done;
          if !i > 0 then Some !i
          else
            (* first word longer than budget: break after it *)
            String.index_opt cur ' '
        end
      in
      match cut with
      | None -> List.rev ((pre ^ cur) :: acc) (* unbreakable, keep as is *)
      | Some i ->
          let head = rtrim (String.sub cur 0 i) in
          let tail =
            ltrim (String.sub cur (i + 1) (String.length cur - i - 1))
          in
          if tail = "" then List.rev ((pre ^ head) :: acc)
          else loop cont_indent tail ((pre ^ head ^ " \\") :: acc)
  in
  loop first_pre body []

let () =
  Arg.parse speclist (fun s -> in_file := s) usage;
  if !in_file = "" then begin
    prerr_endline usage;
    exit 2
  end;
  let ic = open_in !in_file in
  let oc = if !out_file = "" then stdout else open_out !out_file in
  (try
     while true do
       let l = read_logical ic in
       List.iter
         (fun pl ->
           output_string oc pl;
           output_char oc '\n')
         (wrap l)
     done
   with End_of_file -> ());
  close_in ic;
  if !out_file <> "" then close_out oc

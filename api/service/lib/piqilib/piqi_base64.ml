(*

This module is a stripped down version of Base64 module taken from
netstring/netencoding.ml which is a part of ocamlnet-2.2.9

Below is the original copyright notice and the license:


Copyright (c) 2001-2006 Patrick Doane and Gerd Stolpmann

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must
not claim that you wrote the original software. If you use this
software in a product, an acknowledgment in the product documentation
would be appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must
not be misrepresented as being the original software.

3. This notice may not be removed or altered from any source
distribution.
*)

  let b64_pattern plus slash =
    [| 'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M';
       'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
       'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm';
       'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z';
       '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; plus; slash |];;


  let rfc_pattern = b64_pattern '+' '/';;
  let url_pattern = b64_pattern '-' '/';;

  let encode_with_options b64 equal s pos len 
                          linelen first_linelen crlf =
  (* encode using "base64".
   * 'b64': The encoding table, created by b64_pattern.
   * 'equal': The character that should be used instead of '=' in the original
   *          encoding scheme. Pass '=' to get the original encoding scheme.
   * s, pos, len, linelen: See the interface description of encode_substring.
   * first_linelen: The length of the first line.
   *
   * Returns: (s,last_linelen) where [s] is the encoded string, and 
   *   [last_linelen] is the length of the last line
   *)
    assert (Array.length b64 = 64);
    if len < 0 || pos < 0 || pos > String.length s || linelen < 0 then
      invalid_arg "Netencoding.Base64.encode";
    if pos + len > String.length s then
      invalid_arg "Netencoding.Base64.encode";

    let linelen = (linelen asr 2) lsl 2 in
    let first_linelen = (first_linelen asr 2) lsl 2 in

    let l_t = if len = 0 then 0 else ((len - 1) / 3 + 1) * 4 in
    (* l_t: length of the result without additional line endings *)

    let factor = if crlf then 2 else 1 in
    let l_t' = 
      if linelen < 4 then
	l_t
      else
	if l_t <= first_linelen then 
	  ( if l_t = 0 then 0 else l_t + factor )
	else 
	  let n_lines = ((l_t - first_linelen - 1) / linelen) + 2 in
	  l_t + n_lines * factor
    in
    (* l_t': length of the result with CRLF or LF characters *)
    
    let t = String.make l_t' equal in
    let j = ref 0 in
    let q = ref (linelen - first_linelen) in
    for k = 0 to len / 3 - 1 do
      let p = pos + 3*k in
      (* p >= pos >= 0: this is evident
       * p+2 < pos+len <= String.length s:
       *   Because k <= len/3-1
       *         3*k <= 3*(len/3-1) = len - 3
       *   pos+3*k+2 <= pos + len - 3 + 2 = pos + len - 1 < pos + len
       * So it is proved that the following unsafe string accesses always
       * work.
       *)
      let bits = (Char.code (String.unsafe_get s (p))   lsl 16) lor
		 (Char.code (String.unsafe_get s (p+1)) lsl  8) lor
		 (Char.code (String.unsafe_get s (p+2))) in
      (* Obviously, 'bits' is a 24 bit entity (i.e. bits < 2**24) *)
      assert(!j + 3 < l_t');
      String.unsafe_set t !j     (Array.unsafe_get b64 ( bits lsr 18));
      String.unsafe_set t (!j+1) (Array.unsafe_get b64 ((bits lsr 12) land 63));
      String.unsafe_set t (!j+2) (Array.unsafe_get b64 ((bits lsr  6) land 63));
      String.unsafe_set t (!j+3) (Array.unsafe_get b64 ( bits         land 63));
      j := !j + 4;
      if linelen > 3 then begin
	q := !q + 4;
	if !q + 4 > linelen then begin
	  (* The next 4 characters won't fit on the current line. So insert
	   * a line ending.
	   *)
	  if crlf then begin
	    t.[ !j ] <- '\013';
	    t.[ !j+1 ] <- '\010';
	    j := !j + 2;
	  end
	  else begin 
	    t.[ !j ] <- '\010';
	    incr j
	  end;
	  q := 0;
	end;
      end;
    done;
    (* padding if needed: *)
    let m = len mod 3 in
    begin
      match m with
	  0 -> ()
	| 1 ->
            let bits = Char.code (s.[pos + len - 1]) in
	    t.[ !j     ] <- b64.( bits lsr 2);
	    t.[ !j + 1 ] <- b64.( (bits land 0x03) lsl 4);
	    j := !j + 4;
	    q := !q + 4;
	| 2 ->
	    let bits = (Char.code (s.[pos + len - 2]) lsl 8) lor
                       (Char.code (s.[pos + len - 1])) in
	    t.[ !j     ] <- b64.( bits lsr 10);
	    t.[ !j + 1 ] <- b64.((bits lsr  4) land 0x3f);
	    t.[ !j + 2 ] <- b64.((bits lsl  2) land 0x3f);
	    j := !j + 4;
	    q := !q + 4;
	| _ -> assert false
    end;

    (* If required, add another line end: *)

    if linelen > 3 && !q > 0 && len > 0 then begin
      if crlf then begin
	t.[ !j ] <- '\013';
	t.[ !j+1 ] <- '\010';
	j := !j + 2;
      end
      else begin 
	t.[ !j ] <- '\010';
	incr j;
      end;	
    end;

    (t, !q) ;;



  let encode ?(pos=0) ?len ?(linelength=0) ?(crlf=false) s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    let s,_ = 
      encode_with_options rfc_pattern '=' s pos l linelength linelength crlf in
    s
  ;;


  let decode_prefix t pos len p_url p_spaces p_full p_null =
    (* Decodes the prefix of a Base64-encoded string. Returns a triple
     * (s,n,eof) where s is the decoded prefix, and n is the number of 
     * processed characters from t (i.e. the characters pos to pos+n-1 have
     * been processed), and where eof is the boolean flag whether the
     * padding '=' characters at the end of the string have been seen.
     *
     * p_url: accepts strings produced by url_endode
     * p_spaces: accepts spaces in [t] (at the price of reduced speed)
     * p_full: [t] must be a closed encoded string (i.e. no prefix)
     * p_null: [t] must be an encoded null string
     *)

    if len < 0 || pos < 0 || pos > String.length t then
      invalid_arg "Netencoding.Base64.decode";
    if pos + len > String.length t then
      invalid_arg "Netencoding.Base64.decode";

    (* Compute the number of effective characters l_t in 't';
     * pad_chars: number of '=' characters at the end of the string.
     *)
    let l_t, pad_chars =
      if p_spaces then begin
	(* Count all non-whitespace characters: *)
	let c = ref 0 in
	let p = ref 0 in
	for i = pos to pos + len - 1 do
	  match String.unsafe_get t i with
	      (' '|'\t'|'\r'|'\n'|'>') -> ()
	    | ('='|'.') as ch ->
		if ch = '.' && not p_url then
		  invalid_arg "Netencoding.Base64.decode";
		incr c;
		incr p;
		if !p > 2 then
		  invalid_arg "Netencoding.Base64.decode";
		for j = i+1 to pos + len - 1 do
		  match String.unsafe_get t j with
		      (' '|'\t'|'\r'|'\n'|'.'|'=') -> ()
		    | _ ->
			(* Only another '=' or spaces allowed *)
			invalid_arg "Netencoding.Base64.decode";
		done
	    | _ -> incr c
	done;
	!c, !p
      end
      else
	len,
	( if len > 0 then (
	    if String.sub t (len - 2) 2 = "==" || 
	       (p_url && String.sub t (len - 2) 2 = "..") then 2
	    else 
	      if String.sub t (len - 1) 1 = "=" || 
		 (p_url && String.sub t (len - 1) 1 = ".") then 1
	      else
		0
	  )
	  else 0 
	)
    in

    if p_null && l_t <> 0 then invalid_arg "Netencoding.Base64.decode";

    (* Compute the number of characters [l_t] that can be processed now
     * (i.e. the effective prefix)
     *)
    let l_t, pad_chars =
      let m = l_t mod 4 in
      if m = 0 then (
	(l_t, pad_chars)         (* a multiple of 4 *)
      ) else (
	if p_full then invalid_arg "Netencoding.Base64.decode";
	(l_t - m, 0)             (* rounded to a multiple of 4 *)
      )
    in

    let l_s = (l_t / 4) * 3 - pad_chars in
    let s = String.create l_s in

    let decode_char c =
      match c with
	  'A' .. 'Z'  -> Char.code(c) - 65     (* 65 = Char.code 'A' *)
	| 'a' .. 'z'  -> Char.code(c) - 71     (* 71 = Char.code 'a' - 26 *)
	| '0' .. '9'  -> Char.code(c) + 4      (* -4 = Char.code '0' - 52 *)
	| '+'         -> 62
	| '-'         -> if not p_url then 
	                   invalid_arg "Netencoding.Base64.decode";
	                 62
	| '/'         -> 63
	| _           -> invalid_arg "Netencoding.Base64.decode";
    in

    (* Decode all but the last quartet: *)

    let cursor = ref pos in
    let rec next_char() = 
      match t.[ !cursor ] with
	  (' '|'\t'|'\r'|'\n'|'>') -> 
	    if p_spaces then (incr cursor; next_char())
	    else invalid_arg "Netencoding.Base64.decode"
	| c ->
	    incr cursor; c
    in

    if p_spaces then begin
      for k = 0 to l_t / 4 - 2 do
	let q = 3*k in
	let c0 = next_char() in
	let c1 = next_char() in
	let c2 = next_char() in
	let c3 = next_char() in
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let n2 = decode_char c2 in
	let n3 = decode_char c3 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	String.unsafe_set s q     (Char.chr x0);
	String.unsafe_set s (q+1) (Char.chr x1);
	String.unsafe_set s (q+2) (Char.chr x2);
      done;
    end
    else begin
      (* Much faster: *)
      for k = 0 to l_t / 4 - 2 do
	let p = pos + 4*k in
	let q = 3*k in
	let c0 = String.unsafe_get t p in
	let c1 = String.unsafe_get t (p + 1) in
	let c2 = String.unsafe_get t (p + 2) in
	let c3 = String.unsafe_get t (p + 3) in
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let n2 = decode_char c2 in
	let n3 = decode_char c3 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	String.unsafe_set s q     (Char.chr x0);
	String.unsafe_set s (q+1) (Char.chr x1);
	String.unsafe_set s (q+2) (Char.chr x2);
      done;
      cursor := pos + l_t - 4;
    end;

    (* Decode the last quartet: *)

    if l_t > 0 then begin
      let q = 3*(l_t / 4 - 1) in
      let c0 = next_char() in
      let c1 = next_char() in
      let c2 = next_char() in
      let c3 = next_char() in

      if (c2 = '=' & c3 = '=') or (p_url & c2 = '.' & c3 = '.') then begin
	let n0 = decode_char c0 in
	let n1 = decode_char c1 in
	let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	s.[ q ]   <- Char.chr x0;
      end
      else
	if (c3 = '=') or (p_url & c3 = '.') then begin
	  let n0 = decode_char c0 in
	  let n1 = decode_char c1 in
	  let n2 = decode_char c2 in
	  let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	  let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	  s.[ q ]   <- Char.chr x0;
	  s.[ q+1 ] <- Char.chr x1;
	end
	else begin
	  let n0 = decode_char c0 in
	  let n1 = decode_char c1 in
	  let n2 = decode_char c2 in
	  let n3 = decode_char c3 in
	  let x0 = (n0 lsl 2) lor (n1 lsr 4) in
	  let x1 = ((n1 lsl 4) land 0xf0) lor (n2 lsr 2) in
	  let x2 = ((n2 lsl 6) land 0xc0) lor n3 in
	  s.[ q ]   <- Char.chr x0;
	  s.[ q+1 ] <- Char.chr x1;
	  s.[ q+2 ] <- Char.chr x2;
	end

    end
    else cursor := 0;

    (s, !cursor - pos, pad_chars > 0) 
  ;;


  let decode ?(pos=0) ?len ?(url_variant=false) ?(accept_spaces=false) s =
    let l = match len with None -> String.length s - pos | Some x -> x in
    let (s,_,_) = decode_prefix s pos l url_variant accept_spaces true false in
    s
  ;;


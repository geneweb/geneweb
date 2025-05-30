module Compat = Geneweb_compat

type desc =
  | Atext of string
  | Avar of string * string list
  | Atransl of bool * string * string
  | Awid_hei of string
  | Aif of t * t list * t list
  | Aforeach of (string * string list) * t list list * t list
  | Afor of string * t * t * t list
  | Adefine of string * (string * t option) list * t list * t list
  | Aapply of string * (string option * t list) list
  | Alet of string * t list * t list
  | Aop1 of string * t
  | Aop2 of string * t * t
  | Aint of string
  | Ainclude of [ `File of string | `Raw of string ]
  | Apack of t list

and t = { desc : desc; loc : Loc.t }

let equal_pair eq1 eq2 (x1, y1) (x2, y2) = eq1 x1 x2 && eq2 y1 y2

let rec equal_desc d1 d2 =
  match (d1, d2) with
  | Atext s1, Atext s2 -> String.equal s1 s2
  | Atext _, _ | _, Atext _ -> false
  | Avar (s1, l1), Avar (s2, l2) ->
      String.equal s1 s2 && Compat.List.equal String.equal l1 l2
  | Avar _, _ | _, Avar _ -> false
  | Atransl (b1, s11, s21), Atransl (b2, s12, s22) ->
      Bool.equal b1 b2 && String.equal s11 s12 && String.equal s21 s22
  | Atransl _, _ | _, Atransl _ -> false
  | Awid_hei s1, Awid_hei s2 -> String.equal s1 s2
  | Awid_hei _, _ | _, Awid_hei _ -> false
  | Aif (t1, l11, l21), Aif (t2, l12, l22) ->
      equal t1 t2
      && Compat.List.equal equal l11 l12
      && Compat.List.equal equal l21 l22
  | Aif _, _ | _, Aif _ -> false
  | Aforeach ((s1, l11), l21, l31), Aforeach ((s2, l12), l22, l32) ->
      String.equal s1 s2
      && Compat.List.equal String.equal l11 l12
      && Compat.List.equal (Compat.List.equal equal) l21 l22
      && Compat.List.equal equal l31 l32
  | Aforeach _, _ | _, Aforeach _ -> false
  | Afor (s1, t11, t21, l1), Afor (s2, t12, t22, l2) ->
      String.equal s1 s2 && equal t11 t12 && equal t21 t22
      && Compat.List.equal equal l1 l2
  | Afor _, _ | _, Afor _ -> false
  | Adefine (s1, l11, l21, l31), Adefine (s2, l12, l22, l32) ->
      String.equal s1 s2
      && Compat.List.equal
           (equal_pair String.equal (Option.equal equal))
           l11 l12
      && Compat.List.equal equal l21 l22
      && Compat.List.equal equal l31 l32
  | Adefine _, _ | _, Adefine _ -> false
  | Aapply (s1, l1), Aapply (s2, l2) ->
      String.equal s1 s2
      && Compat.List.equal
           (equal_pair (Option.equal String.equal) (Compat.List.equal equal))
           l1 l2
  | Aapply _, _ | _, Aapply _ -> false
  | Alet (s1, l11, l21), Alet (s2, l12, l22) ->
      String.equal s1 s2
      && Compat.List.equal equal l11 l12
      && Compat.List.equal equal l21 l22
  | Alet _, _ | _, Alet _ -> false
  | Aop1 (s1, t1), Aop1 (s2, t2) -> String.equal s1 s2 && equal t1 t2
  | Aop1 _, _ | _, Aop1 _ -> false
  | Aop2 (s1, t11, t21), Aop2 (s2, t12, t22) ->
      String.equal s1 s2 && equal t11 t12 && equal t21 t22
  | Aop2 _, _ | _, Aop2 _ -> false
  | Aint s1, Aint s2 -> String.equal s1 s2
  | Aint _, _ | _, Aint _ -> false
  | Ainclude s1, Ainclude s2 ->
      Loc.equal_source (s1 :> Loc.source) (s2 :> Loc.source)
  | Ainclude _, _ | _, Ainclude _ -> false
  | Apack l1, Apack l2 -> Compat.List.equal equal l1 l2

and equal { desc = d1; loc = l1 } { desc = d2; loc = l2 } =
  Loc.equal l1 l2 && equal_desc d1 d2

let pp_source ppf src =
  match src with
  | `File f -> Fmt.pf ppf "File (%s)" f
  | `Raw s -> Fmt.pf ppf "Raw (%s)" s

let pp_list pp_elt = Fmt.(brackets @@ list ~sep:semi pp_elt)
let pp_pair pp1 pp2 = Fmt.(braces @@ pair ~sep:comma pp1 pp2)

let rec pp_desc ppf t =
  match t with
  | Atext s -> Fmt.pf ppf "Atext (%s)" s
  | Avar (s, sx) -> Fmt.pf ppf "Avar (%s,@ %a)" s (pp_list Fmt.string) sx
  | Atransl (b, s1, s2) -> Fmt.pf ppf "Atransl (%b,@ %s,@ %s)" b s1 s2
  | Awid_hei s -> Fmt.pf ppf "Awid_hei (%s)" s
  | Aif (t, l1, l2) ->
      Fmt.pf ppf "Aif (%a,@ %a,@ %a)" pp t (pp_list pp) l1 (pp_list pp) l2
  | Aforeach ((s, sx), l1, l2) ->
      Fmt.pf ppf "Aforeach ((%s,@ %a),@ %a,@ %a)" s (pp_list Fmt.string) sx
        (pp_list @@ pp_list pp)
        l1 (pp_list pp) l2
  | Afor (s, t1, t2, l) ->
      Fmt.pf ppf "Afor (%s,@ %a,@ %a,@ %a)" s pp t1 pp t2 (pp_list pp) l
  | Adefine (s, l1, l2, l3) ->
      Fmt.pf ppf "Adefine (%s,@ %a,@ %a,@ %a)" s
        (pp_list @@ pp_pair Fmt.string Fmt.(option pp))
        l1 (pp_list pp) l2 (pp_list pp) l3
  | Aapply (s, l) ->
      Fmt.pf ppf "Aapply (%s,@ %a)" s
        (pp_list (pp_pair Fmt.(option string) (pp_list pp)))
        l
  | Alet (s, l1, l2) ->
      Fmt.pf ppf "Alet (%s,@ %a@ %a)" s (pp_list pp) l1 (pp_list pp) l2
  | Aop1 (s, t) -> Fmt.pf ppf "Aop1 (%s,@ %a)" s pp t
  | Aop2 (s, t1, t2) -> Fmt.pf ppf "Aop2 (%s,@ %a,@ %a)" s pp t1 pp t2
  | Aint s -> Fmt.pf ppf "Aint (%s)" s
  | Ainclude s -> Fmt.pf ppf "Ainclude (%a)" pp_source s
  | Apack l -> Fmt.pf ppf "Apack (%a)" (pp_list pp) l

and pp ppf { desc; loc } =
  Fmt.pf ppf "{ desc = %a; loc = %a }" pp_desc desc Loc.pp loc

let pp = Fmt.(box @@ pp)
let[@inline always] mk ?(loc = Loc.dummy) desc = { desc; loc }
let[@inline always] mk_text ?loc x = mk ?loc (Atext x)
let[@inline always] mk_var ?loc x y = mk ?loc (Avar (x, y))
let[@inline always] mk_transl ?loc x y z = mk ?loc (Atransl (x, y, z))
let[@inline always] mk_wid_hei ?loc x = mk ?loc (Awid_hei x)
let[@inline always] mk_foreach ?loc x y z = mk ?loc (Aforeach (x, y, z))
let[@inline always] mk_for ?loc x y z t = mk ?loc (Afor (x, y, z, t))
let[@inline always] mk_if ?loc x y z = mk ?loc (Aif (x, y, z))
let[@inline always] mk_define ?loc x y z t = mk ?loc (Adefine (x, y, z, t))
let[@inline always] mk_apply ?loc x y = mk ?loc (Aapply (x, y))
let[@inline always] mk_let ?loc x y z = mk ?loc (Alet (x, y, z))
let[@inline always] mk_op1 ?loc x y = mk ?loc (Aop1 (x, y))
let[@inline always] mk_op2 ?loc x y z = mk ?loc (Aop2 (x, y, z))
let[@inline always] mk_int ?loc x = mk ?loc (Aint x)
let[@inline always] mk_include ?loc x = mk ?loc (Ainclude x)
let[@inline always] mk_pack ?loc l = mk ?loc (Apack l)

let rec subst_desc sf desc =
  match desc with
  | Atext s -> Atext (sf s)
  | Avar (s, sl) -> (
      let s1 = sf s in
      if
        sl = []
        &&
        try
          let _ = int_of_string s1 in
          true
        with Failure _ -> false
      then Aint s1
      else
        let sl1 = List.map sf sl in
        match String.split_on_char '.' s1 with
        | [ _ ] -> Avar (s1, sl1)
        | s2 :: sl2 -> Avar (s2, sl2 @ sl1)
        | _ -> assert false)
  | Atransl (b, s, c) -> Atransl (b, sf s, c)
  | Awid_hei s -> Awid_hei (sf s)
  | Aif (e, alt, ale) -> Aif (subst sf e, subst_list sf alt, subst_list sf ale)
  | Aforeach ((s, sl), pl, al) ->
      (* Dans le cas d'une "compound variable", il faut la décomposer. *)
      (* Ex: "ancestor.father".family  =>  ancestor.father.family      *)
      let s1 = sf s in
      let sl1 = List.map sf sl in
      let s, sl =
        match
          String.split_on_char '.' s1 (* Templ_parser.compound_var lex *)
        with
        | [ _ ] -> (s1, sl1)
        | s2 :: sl2 -> (s2, List.rev_append (List.rev_map sf sl2) sl1)
        | _ -> assert false
      in
      Aforeach ((s, sl), List.map (subst_list sf) pl, subst_list sf al)
  | Afor (i, min, max, al) ->
      Afor (sf i, subst sf min, subst sf max, subst_list sf al)
  | Adefine (f, xl, al, alk) ->
      Adefine
        ( sf f,
          List.map (fun (x, ast) -> (sf x, Option.map (subst sf) ast)) xl,
          subst_list sf al,
          subst_list sf alk )
  | Aapply (f, all) ->
      Aapply
        ( sf f,
          List.map (fun (x, asts) -> (Option.map sf x, subst_list sf asts)) all
        )
  | Alet (k, v, al) -> Alet (sf k, subst_list sf v, subst_list sf al)
  | Ainclude s -> Ainclude s
  | Aint s -> Aint s
  | Aop1 (op, e) -> Aop1 (op, subst sf e)
  | Aop2 (op, e1, e2) -> Aop2 (sf op, subst sf e1, subst sf e2)
  | Apack l -> Apack (subst_list sf l)

and subst sf { desc; loc } = { desc = subst_desc sf desc; loc }
and subst_list sf l = List.map (subst sf) l

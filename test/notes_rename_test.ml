open Alcotest
open Geneweb

let lk (fn, sn, oc) = (Name.lower fn, Name.lower sn, oc)

let case (ofn, osn) (nfn, nsn) text expected =
  test_case text `Quick (fun () ->
      check string text expected
        (Notes.rename_in_text (lk (ofn, osn, 0)) (nfn, nsn, 0) text))

let remains (ofn, osn) (nfn, nsn) text expected =
  test_case text `Quick (fun () ->
      check bool text expected
        (Notes.old_name_remains (lk (ofn, osn, 0)) (nfn, nsn, 0) text))

let v =
  [
    ( "notes-rename-text",
      [
        case ("Jean", "Dupont") ("Pierre", "Dupont") "Jean Dupont"
          "Pierre Dupont";
        case ("Jean", "Dupont") ("Pierre", "Dupont") "jean dupont"
          "Pierre dupont";
        case ("Jean", "Dupont") ("Pierre", "Dupont")
          "Jean Dupont l\x92a\xEEn\xE9" "Jean Dupont l\x92a\xEEn\xE9";
        case ("Jean", "Dupont") ("Pierre", "Dupont") "JEAN DUPONT, dit Jeannot"
          "PIERRE DUPONT, dit Jeannot";
        case ("Jean", "Dupont") ("Pierre", "Dupont")
          "Jeanne et Jean-Baptiste, enfants de Jean Dupont"
          "Jeanne et Jean-Baptiste, enfants de Pierre Dupont";
        case ("Jean-Pierre", "Dupont") ("Paul", "Dupont") "Jean-Pierre Dupont"
          "Paul Dupont";
        case ("Éloïse", "Dupont") ("Héloïse", "Dupont") "Éloïse Dupont"
          "Héloïse Dupont";
        case ("Henri-xx", "Gouraud") ("Henri", "Gouraud") "Henri-xx Gouraud"
          "Henri Gouraud";
        case ("Jean", "Martin") ("Jean", "Durand")
          "Jean Martin, de Saint-Martin" "Jean Durand, de Saint-Martin";
        case ("Jean", "Martin") ("Jean", "Durand") "Jean Martin dit Martineau"
          "Jean Durand dit Martineau";
        case ("Dupont", "Jean") ("Jean", "Dupont") "Dupont Jean" "Jean Dupont";
        case ("Jean", "Le Goff") ("Jean", "Le Gall") "Jean Le Goff"
          "Jean Le Gall";
        case ("Marie", "Rose") ("Marie", "Rosa") "Marie Rose offre une rose"
          "Marie Rosa offre une rose";
        case ("Jean", "Dupont") ("Pierre", "Dupont") "DUPONT Jean"
          "DUPONT Pierre";
        case ("Jean", "Dupont") ("Jean", "Dupont") "jean DUPONT" "jean DUPONT";
        (* typographic apostrophe is not folded by Name.lower *)
        case ("Jacques", "d’Antoine") ("Jacques", "d'Antoine") "d’Antoine"
          "d'Antoine";
        case ("Jacques", "d’Antoine") ("Jacques", "d'Antoine")
          "la famille d’Antoine" "la famille d'Antoine";
        case ("Jacques", "d’Antoine") ("Jacques", "d'Antoine")
          "D’ANTOINE Jacques" "D'ANTOINE Jacques";
        case
          ("Jacques", "de La Fontaine")
          ("Jacques", "de Lafontaine")
          "le fabuliste de La Fontaine" "le fabuliste de Lafontaine";
        case ("Marie", "Rose") ("Marie", "Rosa") "rose" "Rosa";
      ] );
    ( "notes-old-name-remains",
      [
        remains ("Jean", "Dupont") ("Pierre", "Dupont")
          "Jean Dupont était cultivateur." true;
        remains ("Jean", "Dupont") ("Pierre", "Dupont") "DUPONT Jean, son fils"
          true;
        remains ("Jean", "Dupont") ("Pierre", "Dupont") "voir jean-dupont.html"
          true;
        remains ("Jean", "Dupont") ("Pierre", "Dupont") "Marie Dupont, sa sœur"
          false;
        remains ("Jean", "Dupont") ("Jean", "Dupont-Durand")
          "Jean Dupont-Durand" false;
        remains ("Jean", "Dupont") ("Jean", "Dupont") "Jean Dupont" false;
        (* ' and ’ are equal for the notes search *)
        remains ("Jacques", "d’Antoine") ("Jacques", "d'Antoine")
          "Jacques d’Antoine, notaire" false;
        remains ("Jean", "Dupont") ("Pierre", "Dupont") "<b>Jean</b> Dupont"
          true;
        remains ("?", "Dupont") ("Jean", "Dupont") "un Dupont" false;
        remains ("Jean", "Dupont") ("Jean", "Dupont-Durand")
          "Jean Dupont Durand" false;
        remains ("Jean", "Dupont") ("Pierre", "Dupont")
          "Jean Dupont l\x92a\xEEn\xE9" true;
      ] );
  ]

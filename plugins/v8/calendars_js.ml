open Js_of_ocaml
open Calendars

let _ =
  Js.export "Calendars" begin object%js
    method make day month year = { day ; month ; year ; delta = 0 }
    method day { day ; _ } = day
    method month { month ; _ } = month
    method year { year : _ } = year
    method delta { delta : _ } = delta
    method gregorianOfSdn = gregorian_of_sdn
    method julianOfSdn = julian_of_sdn
    method frenchOfSdn = french_of_sdn
    method hebrewOfSdn = hebrew_of_sdn
    method sdnOfGregorian = sdn_of_gregorian
    method sdnOfJulian = sdn_of_julian
    method sdnOfFrench = sdn_of_french
    method sdnOfHebrew = sdn_of_hebrew
  end end

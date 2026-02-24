module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let clean_declension fn sn =
  ( (try
       let i = String.index fn ':' in
       String.sub fn 0 i
     with Not_found -> fn),
    try
      let i = String.index sn ':' in
      String.sub sn 0 i
    with Not_found -> sn )

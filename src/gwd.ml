open Geneweb

module Request = Request.Make (struct let handler = RequestHandler.defaultHandler end)
module Main = GwDaemon.Make (Wserver) (Request)

let _ =
  print_endline "Compilé le vendredi 20 juillet 2018, 16:09:18 (UTC+0200)" ;
  Main.run ()

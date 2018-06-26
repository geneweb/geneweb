open Geneweb

module Request = Request.Make (struct let handler = RequestHandler.defaultHandler end)
module Main = GwDaemon.Make (Wserver) (Request)

let _ = Main.run ()

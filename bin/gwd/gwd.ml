open Geneweb
open Gwdlib

let handler =
  { RequestHandler.defaultHandler
    with fallback = begin fun mode self conf base ->
        if mode = "SANDBOX" then begin
        let models = Gwxjg.Data.default_env conf base in
        JgInterp.render ~conf ~file:"SANDBOX.html.jingoo" ~models
        end else self.incorrect_request self conf base
    end
}

module Request = Request.Make (struct let handler = handler end)
module Main = GwDaemon.Make (Request)

let _ =
  Main.run ()

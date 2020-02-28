(* $Id: request.mli,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

module type MakeIn = sig
  val handler : RequestHandler.handler
end

module type MakeOut = sig
  val treat_request_on_base : Config.config -> unit
  val treat_request_on_nobase : Config.config -> unit
end

module Make (_ : MakeIn) : MakeOut

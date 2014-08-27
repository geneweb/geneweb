(* $Id: api_conf.ml,v 0.1 2014-08-27 10:32:58 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

value mode_api = ref False;
value set_mode_api () = mode_api.val := True;

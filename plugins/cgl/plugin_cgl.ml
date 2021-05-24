open Geneweb
open Config

let ns = "cgl"

let () =
  Gwd_lib.GwdPlugin.register_se ~ns @@ fun _assets conf _base ->
  if Util.p_getenv conf.env "cgl" = Some "on" then begin
    let buffer_status = ref None in
    let buffer_headers = ref [] in
    let buffer_body = Buffer.create 1023 in
    let previous_status = conf.output_conf.status in
    let previous_header = conf.output_conf.header in
    let previous_body = conf.output_conf.body in
    let previous_file = conf.output_conf.file in
    let previous_flush = conf.output_conf.flush in
    let status s = buffer_status := Some s in
    let header s = buffer_headers := s :: !buffer_headers in
    let body s = Buffer.add_string buffer_body s in
    let file fname ctype res = false in 
    let flush () =
      conf.output_conf <- { status = previous_status
                          ; header = previous_header
                          ; body = previous_body
                          ; file = previous_file
                          ; flush = previous_flush
                          } ;
      (match !buffer_status with Some s -> Output.status conf s | None -> ());
      List.iter (Output.header conf "%s") (List.rev !buffer_headers) ;
      let open Markup in
      buffer buffer_body
      |> parse_html
      |> signals
      |> map begin function
        | `Start_element (("http://www.w3.org/1999/xhtml", "a"), _) ->
          `Start_element (("http://www.w3.org/1999/xhtml", "span"), [])
        | x -> x
      end
      |> write_html
      |> to_string
      |> Output.print_string conf ;
      Output.flush conf ;
      Buffer.reset buffer_body ;
    in
    conf.output_conf <- { status ; header ; body ; file ; flush } ;
  end

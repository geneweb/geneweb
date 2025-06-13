module Header = struct
  let get ~request header =
    Mutil.extract_param (Printf.sprintf "%s:" header) '\n' request
end

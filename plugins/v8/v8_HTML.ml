type doc = element list
and element = Tag of string * attribute list * element list
            | SelfTag of string * attribute list
            | Pcdata of string
and attribute = string * string

module ATTR = struct

  let attr attr s : attribute = (attr, s)

  let class_ = attr "class"
  let colspan = attr "colspan"
  let for_ = attr "for"
  let href = attr "href"
  let id = attr "id"
  let list = attr "list"
  let name = attr "name"
  let rel = attr "rel"
  let tabindex = attr "tabindex"
  let type_ = attr "type"
  let value = attr "value"
  
end

module TAG = struct

  let tag tag ?(attr = []) content : element = Tag (tag, attr, content)
  let self tag ?(attr = []) () = SelfTag (tag, attr)
  let pcdata s = Pcdata s

  let a = tag "a"
  let body = tag "body"
  let button = tag "button"
  let br = self "br"
  let datalist = tag "datalist"
  let div = tag "div"
  let h1 = tag "h1"
  let em = tag "em"
  let h2 = tag "h2"
  let h3 = tag "h3"
  let h4 = tag "h4"
  let h5 = tag "h5"
  let h6 = tag "h6"
  let head = tag "head"
  let html = tag "html"
  let fieldset = tag "fieldset"
  let form = tag "form"
  let input = self "input"
  let label = tag "label"
  let li = tag "li"
  let link = self "link"
  let meta = self "meta"
  let option = self "option"
  let pre = tag "pre"
  let p = tag "pre"
  let span = tag "span"
  let strong = tag "strong"
  let table = tag "table"
  let tbody = tag "body"
  let td = tag "td"
  let textarea = tag "textarea"
  let th = tag "th"
  let thead = tag "thead"
  let title = tag "title"
  let tr = tag "tr"
  let ul = tag "ul"

  let input_checkbox ?(attr = []) () = input ~attr:(ATTR.type_ "checkbox" :: attr) ()
  let input_radio ?(attr = []) () = input ~attr:(ATTR.type_ "radio" :: attr) ()
  let input_text ?(attr = []) () = input ~attr:(ATTR.type_ "text" :: attr) ()
  let input_hidden ?(attr = []) () = input ~attr:(ATTR.type_ "hidden" :: attr) ()

end

module RENDER = struct

  let r_attrs conf =
    List.iter begin fun (k, v) ->
      Geneweb.Output.print_string conf " " ;
      Geneweb.Output.print_string conf k ;
      Geneweb.Output.print_string conf "=\"" ;
      Geneweb.Output.print_string conf v ;
      Geneweb.Output.print_string conf "\"" ;
    end
  
  let rec r conf =
    List.iter begin function
      | Tag (tag, attrs, contents) ->
        Geneweb.Output.print_string conf "<" ;
        Geneweb.Output.print_string conf tag ;
        r_attrs conf attrs ;
        Geneweb.Output.print_string conf ">" ;
        r conf contents ;
        Geneweb.Output.print_string conf "</" ;
        Geneweb.Output.print_string conf tag ;
        Geneweb.Output.print_string conf ">" ;
      | SelfTag (tag, attrs) ->
        Geneweb.Output.print_string conf "<" ;
        Geneweb.Output.print_string conf tag ;
        r_attrs conf attrs ;
        Geneweb.Output.print_string conf ">" ;
      | Pcdata s ->
        Geneweb.Output.print_string conf s ;
    end

end

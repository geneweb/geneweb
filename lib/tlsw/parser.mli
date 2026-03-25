(** {1 TLSW: Text Language Stolen from Wikipedia}

    This module provides a parser for the Wikipedia-inspired markup syntax used
    by GeneWeb.

    {2 Headers}
    Use equal signs to denote section hierarchy. Six levels are supported.
    {v
    = Level 1 =
    == Level 2 ==
    ...
    ====== Level 6 ======
    v}

    {2 Horizontal Rules}
    Four or more hyphens create a horizontal rule.
    {v
    ----
    ---------...
    v}

    {2 Tables of Contents}
    Control the visibility and style of the Table of Contents:
    {v
    __TOC__
    __SHORT_TOC__
    __NOTOC__
    v}

    {2 Lists}
    Use asterisks for unordered lists. Increase the number of asterisks for
    nesting. For example:
    {v
    * item
    * item
    ** nested item
    ** nested item
    * item
    v}

    Use the hash symbol for ordered lists. Increase the number of hashes for
    nesting. For example:
    {v
    # item
    # item
    ## nested item
    ## nested item
    # item
    v}

    Ordered and unordered lists can be mixed:
    {v
    # item
    * item
    ** nested item
    * item
    # item
    ## nested item
    # item
    v}

    {2 Text Formatting}
    Apply styles using quotes, underscores, or curly braces:
    {v
    ''italic''
    '''bold'''
    '''''bold + italic'''''
    __underline__
    ~~strike~~
    {highlight}
    v}

    {2 Links and References}

    {3 Person Pages}
    Link to a person's page using double brackets. The occurrence (occ) and
    display text are optional.
    {v
    [[first name/surname]]
    [[first name/surname/occ]]
    [[first name/surname/occ/text]]
    v}

    {3 Bibliographic Notes}
    Reference bibliographic notes using triple brackets. The display text is
    optional.
    {v
    [[[dir1:dir2:file]]]
    [[[dir1:dir2:file/text]]]
    v}
    {b Note:} Use the colon symbol [:] as a path separator instead of the
    standard forward slash [/].

    {3 Wizard Pages}
    Reference wizard pages using the [w:] prefix:
    {v
    [[w:dir1:dir2:file]]
    [[w:dir1:dir2:file/text]]
    v}

    {3 Images}
    Embed images as follows. The display text is optional, and [width] must be a
    valid CSS size.
    {v
    [[image:dir1:dir2:file]]
    [[image:dir1:dir2:file/text]]
    [[image:dir1:dir2:file/text/width]]
    v}
    As with notes, use the colon [:] as the path separator regardless of your
    platform's native directory separator. *)

type error_handler = loc:Geneweb_loc.t -> string -> unit

val parse : recover:bool -> on_err:error_handler -> string -> Ast.block list
(** [parse ~on_err s] parses the input [s] for the TLSW syntax. *)

val parse_links : on_err:error_handler -> string -> Ast.link list
(** [parse_links ~on_err s] *)

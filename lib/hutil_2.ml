let header ?(error = false) ?(templ = "home_full") conf base title =
  Perso.interp_templ templ conf base (Gwdb.poi base Gwdb.dummy_iper);
  Output.print_sstring conf "<div class=\"container\">";
  Output.print_sstring conf (if error then "<h1 class=\"error\">" else "<h1>");
  title false;
  Output.print_sstring conf "</h1>"

type fd

external file_descr_to_fd : Unix.file_descr -> fd
  = "geneweb_win32_file_descr_to_fd"

external file_descr_of_fd : fd -> Unix.file_descr
  = "geneweb_win32_file_descr_of_fd"

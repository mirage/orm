let open_db ?(rm=true) fn name =
  if Sys.file_exists name && rm then Sys.remove name;
  try fn name
  with exn -> Printexc.print_backtrace stdout; raise exn


let open_db ?(rm=true) fn name =
  if Sys.file_exists name && rm then Sys.remove name;
  fn name


let open_db ?(rm=true) fn name =
	let db = Orm.Db.create name in
	if rm then
		Orm.Db.remove db;
	try fn db
	with exn -> Printexc.print_backtrace stdout; raise exn

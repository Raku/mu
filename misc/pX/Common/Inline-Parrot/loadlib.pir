.sub test :main
	.local pmc libparrotside
	.local pmc send_msg
	.local pmc send_pmc
	libparrotside = loadlib "libparrotside"
	unless libparrotside goto NOT_LOADED
	dlfunc send_msg,libparrotside,"send_msg", "v"
	dlfunc send_pmc,libparrotside,"send_pmc", "iJPt"
	send_msg( )
	.local pmc data
	data = new .PerlInt
	data = 5
	send_pmc(data,"Inline::Parrot::data")
	end
	NOT_LOADED:
	say "Cannot load libparrotside"
.end

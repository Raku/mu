.sub test :main
	.local pmc libparrotside
	.local pmc hello_world
	libparrotside = loadlib "libparrotside"
	unless libparrotside goto NOT_LOADED
	dlfunc hello_world,libparrotside,"hello_world", "v"
	hello_world( )
	end
	NOT_LOADED:
	say "Cannot load libparrotside"
.end

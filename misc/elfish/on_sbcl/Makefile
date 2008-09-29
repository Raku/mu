

elfx:: have_parser_cache
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src Elf_SBCL.pm

#XXX no main() call yet, so CommandLine must come after last file...
# but doesnt here.
#	ELF_STD_RED_RUN=../../STD_red/STD_red_run ../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src Elf.pm EmitSBCL.pm



have_parser_cache:
	@perl -we 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)


clean:
	rm -rf lib-cl/tar
	rm -rf lib-cl/site
	rm -rf lib-cl/systems

libraries:
	-mkdir lib-cl
	-mkdir lib-cl/tar
	-mkdir lib-cl/site
	-mkdir lib-cl/systems
	(cd lib-cl/tar; wget http://common-lisp.net/project/cl-plus-ssl/download/trivial-gray-streams.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/flexi-streams.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-interpol.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-unicode.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-ppcre.tar.gz)
	(cd lib-cl/site; tar xzf ../tar/trivial-gray-streams.tar.gz)
	(cd lib-cl/site; tar xzf ../tar/flexi-streams.tar.gz)
	(cd lib-cl/site; tar xzf ../tar/cl-interpol.tar.gz)
	(cd lib-cl/site; tar xzf ../tar/cl-unicode.tar.gz)
	(cd lib-cl/site; tar xzf ../tar/cl-ppcre.tar.gz)
	(cd lib-cl/site; mv trivial-gray-streams* trivial-gray-streams)
	(cd lib-cl/site; mv flexi-streams* flexi-streams)
	(cd lib-cl/site; mv cl-interpol* cl-interpol)
	(cd lib-cl/site; mv cl-unicode* cl-unicode)
	(cd lib-cl/site; mv cl-ppcre* cl-ppcre)
	(cd lib-cl/systems; ln -s ../site/trivial-gray-streams/trivial-gray-streams.asd)
	(cd lib-cl/systems; ln -s ../site/flexi-streams/flexi-streams.asd)
	(cd lib-cl/systems; ln -s ../site/cl-interpol/cl-interpol.asd)
	(cd lib-cl/systems; ln -s ../site/cl-unicode/cl-unicode.asd)
	(cd lib-cl/systems; ln -s ../site/cl-ppcre/cl-ppcre.asd)
	./lib-cl_compile.lisp


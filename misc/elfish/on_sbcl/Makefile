
ELF=../../elf/elf_h
ELFDIR=../../elf/elf_h_src
TMP=deleteme
SBCL=sbcl --dynamic-space-size 1800

elfx:: have_parser_cache
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src -e 'use Elf_wo_main' EmitSBCL.pm -e elf_main

elfcl:: have_parser_cache
	${ELF} -I ${ELFDIR} EmitSBCL.pm -x -o elfcl Elf_SBCL.pm
	time ${SBCL} --eval '(compile-file "elfcl")' --eval '(quit)' >& elfcl.out

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

check: have_parser_cache
	-mkdir ${TMP}
	-rm ${TMP}/[ab]*
	# Create a p5 elf.
	${ELF} -I ${ELFDIR} -x -o ${TMP}/a0 ${ELFDIR}/Elf.pm
	# Create a CL elf.
	${ELF} -I ${ELFDIR} EmitSBCL.pm -x -o ${TMP}/b0 Elf_SBCL.pm
	# Compile it, with output to log.
	${SBCL} --disable-debugger --eval '(compile-file "${TMP}/b0")' --eval '(quit)' > ${TMP}/b0.log 2>&1
	# Run it.
	chmod a+x ${TMP}/b0
	${TMP}/b0 -e 'say 3'
	# Check CL bootstrap.
	${TMP}/b0 -I ${ELFDIR} -x -o ${TMP}/b1 Elf_SBCL.pm
	diff ${TMP}/b0 ${TMP}/b1
	# Compile a p5 elf with a CL one.
	${TMP}/b0 -I ${ELFDIR} -e 'use EmitSimpleP5' -x -o ${TMP}/a1 -e 'use Elf'
	# Was it identical?
	diff ${TMP}/a0 ${TMP}/a1

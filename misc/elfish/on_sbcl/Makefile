
ELF=../../elf/elf_h
ELFDIR=../../elf/elf_h_src
TMP=deleteme
SBCL=sbcl --dynamic-space-size 1800
CLISP=clisp

elfcl.lisp:: have_parser_cache
	${ELF} -I ${ELFDIR} EmitSBCL.pm -x -o elfcl.lisp Elf_SBCL.pm
	chmod a+x elfcl.lisp

elfx:: have_parser_cache
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src -e 'use Elf_wo_main' EmitSBCL.pm -e elf_main
	chmod a+x elfx

elfcl-sbcl:: elfcl.lisp
	${SBCL} --eval '(compile-file "elfcl.lisp")' --eval '(quit)'

elfcl-clisp:: elfcl.lisp
	${CLISP} -c elfcl.lisp

elfcl-ccl:: elfcl.lisp
	#ccl???


clean:
	-rm elfx elfcl.lisp
	-rm elfcl.fasl elfcl.out #sbcl
	-rm elfcl.fas  #clisp
	-rm -rf lib-cl/tar
	-rm -rf lib-cl/for-*
	-rm -rf deleteme/

get-libraries:
	-mkdir lib-cl
	-mkdir lib-cl/tar
	-rm -rf lib-cl/for-*
	(cd lib-cl/tar; wget http://common-lisp.net/project/cl-plus-ssl/download/trivial-gray-streams.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/flexi-streams.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-interpol.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-unicode.tar.gz)
	(cd lib-cl/tar; wget http://weitz.de/files/cl-ppcre.tar.gz)

setup-sbcl:
	_IMPL_=sbcl make _libraries_for_IMPL_
	sbcl --noinform --load ./lib-cl_compile.lisp --eval "(quit)"

setup-clisp:
	_IMPL_=clisp make _libraries_for_IMPL_
	clisp -norc ./lib-cl_compile.lisp

setup-ccl:
	_IMPL_=ccl make _libraries_for_IMPL_
	#ccl???

_libraries_for_IMPL_:
	-mkdir lib-cl/for-${_IMPL_}
	-mkdir lib-cl/for-${_IMPL_}/systems
	-mkdir lib-cl/for-${_IMPL_}/site
	(cd lib-cl/for-${_IMPL_}/site; tar xzf ../../tar/trivial-gray-streams.tar.gz)
	(cd lib-cl/for-${_IMPL_}/site; tar xzf ../../tar/flexi-streams.tar.gz)
	(cd lib-cl/for-${_IMPL_}/site; tar xzf ../../tar/cl-interpol.tar.gz)
	(cd lib-cl/for-${_IMPL_}/site; tar xzf ../../tar/cl-unicode.tar.gz)
	(cd lib-cl/for-${_IMPL_}/site; tar xzf ../../tar/cl-ppcre.tar.gz)
	(cd lib-cl/for-${_IMPL_}/site; mv trivial-gray-streams* trivial-gray-streams)
	(cd lib-cl/for-${_IMPL_}/site; mv flexi-streams* flexi-streams)
	(cd lib-cl/for-${_IMPL_}/site; mv cl-interpol* cl-interpol)
	(cd lib-cl/for-${_IMPL_}/site; mv cl-unicode* cl-unicode)
	(cd lib-cl/for-${_IMPL_}/site; mv cl-ppcre* cl-ppcre)
	(cd lib-cl/for-${_IMPL_}/systems; ln -s ../site/trivial-gray-streams/trivial-gray-streams.asd)
	(cd lib-cl/for-${_IMPL_}/systems; ln -s ../site/flexi-streams/flexi-streams.asd)
	(cd lib-cl/for-${_IMPL_}/systems; ln -s ../site/cl-interpol/cl-interpol.asd)
	(cd lib-cl/for-${_IMPL_}/systems; ln -s ../site/cl-unicode/cl-unicode.asd)
	(cd lib-cl/for-${_IMPL_}/systems; ln -s ../site/cl-ppcre/cl-ppcre.asd)


check: have_parser_cache
	-mkdir ${TMP}
	-rm ${TMP}/[ab]*
	# Create a p5 elf.
	${ELF} -I ${ELFDIR} -x -o ${TMP}/a0 ${ELFDIR}/Elf.pm
	# Create a CL elf.
	${ELF} -I ${ELFDIR} EmitSBCL.pm -x -o ${TMP}/b0 Elf_SBCL.pm
	# Compile the CL elf.
	${SBCL} --disable-debugger --eval '(compile-file "${TMP}/b0")' --eval '(quit)' > ${TMP}/b0.log 2>&1
	# Run the CL elf.
	chmod a+x ${TMP}/b0
	${TMP}/b0 -e 'say "hello"' 2>/dev/null
	# Check bootstrap - CL elf compiles a CL elf.
	${TMP}/b0 -I ${ELFDIR} -x -o ${TMP}/b1 Elf_SBCL.pm 2>/dev/null
	# Was it identical?
	diff ${TMP}/b0 ${TMP}/b1
	# Check cross-bootstrap - CL elf compiles a P5 elf.
	${TMP}/b0 -I ${ELFDIR} -e 'use EmitSimpleP5' -x -o ${TMP}/a1 -e 'use Elf' 2>/dev/null
	# Was it identical?
	diff ${TMP}/a0 ${TMP}/a1

have_parser_cache:
	@perl -we 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)



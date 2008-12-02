
ELF=../../elf/elf_h
ELFDIR=../../elf/elf_h_src
ELFBLUEDEV=-I ../../elf/elf_h_src -e 'use Elf_wo_main' IRx1_FromAST2.pm -e '$$*ast2ir_0=$$*ast2ir_1; $$*parser0=undef' Parser2.pm -e elf_main
ELFBLUECOMP=-I . -I ../../elf/elf_h_src -e 'use Elf_wo_main' IRx1_FromAST2.pm Parser2.pm -e elf_main
TMP=deleteme

elfblue:: have_parser_cache
	./IRx1_FromAST2_create.pl
	../../elf/elf_h -x -o ./elfblue ${ELFBLUEDEV}

does_gimme5_memory_problem_still_exist:
	bash -c "ulimit -v 2000000; ./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/PrimitivesP5.pm"
	@echo No! Time to remove the work-around PrimitivesP5.pm mutant.

known_working_STD:
	# r24080 broke.
	rm -rf lex
	(cd ../../../src/perl6; svn up -r24067; make distclean; make)
	# done.

update_STD:
	rm -rf lex
	(cd ../../../src/perl6; svn up; make distclean; make)
	# done.

check: have_parser_cache
	# Remove STD.pm/gimme5 cruft.
	-rm -rf lex
	# Clean up from any previous runs.
	-mkdir ${TMP}
	-rm ${TMP}/[a]*
	#
	# Create a STD_blue-using elf hybrid.
	${ELF} -x -o ${TMP}/a0 ${ELFBLUECOMP}
	# Create a STD_blue elf with it.
	${TMP}/a0 -x -o ${TMP}/a1 ${ELFBLUECOMP}
	# Try self-compiling.
	${TMP}/a1 -x -o ${TMP}/a2 ${ELFBLUECOMP}
	# Were they the same?
	diff ${TMP}/a0 ${TMP}/a2
	diff ${TMP}/a1 ${TMP}/a2
	#
	# Use it to compile a (tweaked) normal elf.
	# Tweaked: ./PrimitivesP5.pm substitued using -I . ,
	# since gimme5 requires too much memory for the real one.
	${TMP}/a1 -x -o ${TMP}/a5 -I . -I ${ELFDIR} ${ELFDIR}/Elf.pm
	# Create a reference elf.
	${ELF}    -x -o ${TMP}/a6 -I . -I ${ELFDIR} ${ELFDIR}/Elf.pm
	# Were they the same?
	diff ${TMP}/a5 ${TMP}/a6
	@echo ok


have_parser_cache:
	@perl -e 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)

regression_debug:
	-rm ${TMP}/x?
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/CommandLine.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/CommandLine.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/Compiler.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/Compiler.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/EmitSimpleP5.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/EmitSimpleP5.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/IRx1_Analysis.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/IRx1_Analysis.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/IRx1_FromAST.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/IRx1_FromAST.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/IRx1_Nodes.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/IRx1_Nodes.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/Match.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/Match.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/Parser.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/Parser.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/Prelude.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/Prelude.pm
	diff ${TMP}/x0 ${TMP}/x1
	#./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/PrimitivesP5.pm
	#${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/PrimitivesP5.pm
	#diff ${TMP}/x0 ${TMP}/x1
	./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ./PrimitivesP5.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/PrimitivesP5.pm
	diff ${TMP}/x0 ${TMP}/x1
	@echo ok


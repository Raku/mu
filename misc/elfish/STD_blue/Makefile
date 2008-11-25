
ELF=../../elf/elf_h
ELFDIR=../../elf/elf_h_src
TMP=deleteme

elfx:: have_parser_cache
	./IRx1_FromAST2_create.pl
	../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src -e 'use Elf_wo_main' IRx1_FromAST2.pm Parser2.pm -e elf_main

tst:
	-rm ${TMP}/x?
	./elfx -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/EmitSimpleP5.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/EmitSimpleP5.pm
	-diff ${TMP}/x0 ${TMP}/x1

regression_check:
	-rm ${TMP}/x?
	./elfx -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/CommandLine.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/CommandLine.pm
	diff ${TMP}/x0 ${TMP}/x1
	./elfx -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/Compiler.pm
	${ELF} -I ${ELFDIR} -x -o ${TMP}/x0 ${ELFDIR}/Compiler.pm
	diff ${TMP}/x0 ${TMP}/x1
	@echo ok

check: have_parser_cache
	# Remove STD.pm/gimme5 cruft.
	-rm -rf lex
	# Remove remains of previous runs.
	-mkdir ${TMP}
	-rm ${TMP}/[a]*
	# Create a STD_blue based elf.
	make elfx
	# Use it to compile a normal elf.
	./elfx -I ${ELFDIR} -x -o ${TMP}/a1 ${ELFDIR}/Elf.pm
	# Test the normal elf by using it to self-compile.
	./elfx -I ${ELFDIR} -x -o ${TMP}/a2 ${ELFDIR}/Elf.pm
	# Create a reference elf.
	${ELF} -I ${ELFDIR} -x -o ${TMP}/a0 ${ELFDIR}/Elf.pm
	# Were they the same?
	diff ${TMP}/a0 ${TMP}/a2
	@echo ok


have_parser_cache:
	@perl -e 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)

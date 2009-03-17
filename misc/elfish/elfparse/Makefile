ELF=../../elf/elf_h
ELFDIR=../../elf/elf_h_src
SRCP6=./pugs_src_perl6
STDPM=./pugs_src_perl6/STD.pm
export STD_RED_CACHEDIR  := ./deleteme_cache_red
export STD_BLUE_CACHEDIR := ./deleteme_cache_blue

DEPS_FOR_STDBLUE_USERS=${SRCP6}/STD.pmc ./STD_blue_run

ELFBLUEDEV=-I ${ELFDIR} -e 'use Elf_wo_main' IRx1_FromAST2.pm -e '$$*ast2ir_0=$$*ast2ir_1; $$*parser0=undef' Parser2.pm
ELFBLUECOMP=-I . -I  ${ELFDIR} -e 'use Elf_wo_main' IRx1_FromAST2.pm Parser2.pm -e elf_main
ELFBLUEDEV_DEPS=${DEPS_FOR_STDBLUE_USERS} ${ELF} IRx1_FromAST2.pm Parser2.pm

ELFRXDEV=-I . -I ${ELFDIR} -e 'use Elf_wo_main' IRx1_FromAST2.pm -e '$$*ast2ir_0=$$*ast2ir_1; $$*parser0=undef' Parser2.pm nodes.pm analysis.pm emit5.pm -e '_inline_p5("require \"./rx_prelude_p5.pm\";")' 
ELFRXDEV_DEPS=${ELFBLUEDEV_DEPS} nodes.pm analysis.pm emit5.pm remains_of_Regexp_ModuleA.pm rx_prelude.pm
# rx_prelude.pm not rx_prelude_p5.pm

TMP=deleteme

default:
	perl -ne '/^(\w+):/ && print "make $$1\n"' < Makefile


is_perl_recent_enough: STD_green_run
	# is_perl_recent_enough?  If you get a non-terminating deep recursion, then no.
	./STD_green_run --start=number -e 3
	@echo ok

all: elfblue elfrx STD_green_run elfdev


clean:: clean_not_caches
	rm -rf lex lex.is_current ${STD_RED_CACHEDIR} ${STD_RED_CACHEDIR}.is_current ${STD_BLUE_CACHEDIR} ${STD_BLUE_CACHEDIR}.is_current

clean_not_caches:
	rm -rf ${TMP} elfblue elfrx elfdev STD_green_run IRx1_FromAST2.pm nodes.pm rx_prelude.pm rx_prelude_p5.pm std.pm.p5 STD_green_run.pm.p5


# STD_blue

${SRCP6}/STD.pmc: ${SRCP6}/STD.pm ${SRCP6}/gimme5
	(cd ${SRCP6}; make fast)

lex.is_current: ${SRCP6}/STD.pmc
	rm -rf lex lex.is_current
	mkdir lex
	touch lex.is_current

${STD_BLUE_CACHEDIR}.is_current: lex.is_current ${DEPS_FOR_STDBLUE_USERS}
	rm -rf ${STD_BLUE_CACHEDIR}
	mkdir ${STD_BLUE_CACHEDIR}
	touch ${STD_BLUE_CACHEDIR}.is_current

${STD_RED_CACHEDIR}.is_current: ${ELF}
	rm -rf ${STD_RED_CACHEDIR}
	mkdir ${STD_RED_CACHEDIR}
	touch ${STD_RED_CACHEDIR}.is_current


# elfblue

IRx1_FromAST2.pm: IRx1_FromAST2_create.pl
	./IRx1_FromAST2_create.pl

elfblue: ${ELF} ${ELFBLUEDEV_DEPS}
	make ${STD_RED_CACHEDIR}.is_current 
	${ELF} -x -o ./elfblue ${ELFBLUEDEV} -e elf_main
	make ${STD_BLUE_CACHEDIR}.is_current 

# elfrx

elfrx: ${ELF} ${ELFRXDEV_DEPS}
	make ${STD_RED_CACHEDIR}.is_current 
	make ${STD_BLUE_CACHEDIR}.is_current 
	echo "1;" > rx_prelude_p5.pm
	${ELF} -x -o ./elfrx ${ELFRXDEV} -e elf_main
	./elfrx -o rx_prelude_p5.pm -s rx_prelude.pm

nodes.pm: nodes_create.pl
	./nodes_create.pl

rx_prelude.pm: ${ELF} rx_prelude_create.p6
	make ${STD_RED_CACHEDIR}.is_current
	${ELF} rx_prelude_create.p6 > rx_prelude.pm

rx_prelude_p5.pm: elfrx rx_prelude.pm
	./elfrx -o rx_prelude_p5.pm -s rx_prelude.pm

# STD_green

std.pm.p5: elfrx std.pm
	./elfrx -s -o std.pm.p5 std.pm

STD_green_run.pm.p5: elfrx STD_green_run.pm
	./elfrx -s -o STD_green_run.pm.p5 STD_green_run.pm

STD_green_run: elfrx std.pm STD_green_run.pm std.pm.p5 STD_green_run.pm.p5
	${ELF} -x -o STD_green_run ${ELFRXDEV} -e '_inline_p5("require \"./std.pm.p5\";")' -e '_inline_p5("require \"./STD_green_run.pm.p5\";")'

elfdev: Parser3.pm STD_green_run
	${ELF} -x -o ./elfdev ${ELFRXDEV} Parser3.pm -e '_inline_p5("require \"./std.pm.p5\";")' -e elf_main

elfdev1: Parser3.pm STD_green_run
	./elfrx -x -o ./elfdev1 ${ELFRXDEV} Parser3.pm -e '_inline_p5("require \"./std.pm.p5\";")' -e elf_main

# Test

check: check_rx_on_re check_STD_blue

check_rx_on_re: nodes.pm rx_prelude_p5.pm
	# check_rx_on_re - The rx_on_re core still passes its test suite.
	perl -e 'use Regexp::Common;1' # dependency check
	make ${STD_RED_CACHEDIR}.is_current 
	${ELF} test.pm > tst_1 2> tst_2
	diff t_baseline.result tst_1
	@echo ok
	# check_rx_on_re passed.
	#

check_std_rx_on_re: elfrx
	rm -rf ${STD_BLUE_CACHEDIR} ${STD_BLUE_CACHEDIR}.is_current
	# Create cache dirs.  This is WRONG.  Env vars should be unset instead.
	make ${STD_BLUE_CACHEDIR}.is_current  #XXX
	if [ "${STD_BLUE_CACHEDIR}" != "" ];then echo;echo "WARNING: Using STD_BLUE_CACHEDIR: " ${STD_BLUE_CACHEDIR};echo; fi
	-rm -rf lex  # Reset gimme5.
	ulimit -v 1000000; ./elfrx test_blue.pm > tst_1b 2> tst_2b
	diff t_test_blue.result tst_1b
	#diff t_baseline.result tst_1b

rerun_std_rx_on_re:
	ulimit -v 1000000; ./elfrx test_blue.pm > tst_1b 2> tst_2b
	diff t_test_blue.result tst_1b


check_STD_blue: ${ELF} ${ELFBLUEDEV_DEPS}
	rm -rf ${STD_RED_CACHEDIR} ${STD_RED_CACHEDIR}.is_current 
	rm -rf ${STD_BLUE_CACHEDIR} ${STD_BLUE_CACHEDIR}.is_current 
	# Create cache dirs.  This is WRONG.  Env vars should be unset instead.
	make ${STD_RED_CACHEDIR}.is_current  #XXX
	make ${STD_BLUE_CACHEDIR}.is_current  #XXX
	# check_STD_blue - Elf can be compiled using STD.pm/gimme5 parsing.
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



# Development hacks

does_gimme5_memory_problem_still_exist:
	bash -c "ulimit -v 2000000; ./elfblue -I ${ELFDIR} -x -o ${TMP}/x1 ${ELFDIR}/PrimitivesP5.pm"
	@echo No! Time to remove the work-around PrimitivesP5.pm mutant.

elfblue_regression_debug:
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

have_STD_red_cache:
	@perl -e 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)

have_STD_blue_cache:
	@perl -e 'if(!-d shift){print STDERR "\nDefine STD_BLUE_CACHEDIR for faster compiles.\n\n";}' $(STD_BLUE_CACHEDIR)

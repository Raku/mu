
check:
	../../elf/elf_h test.pm > tst_1 2> tst_2
	diff t_baseline.result tst_1
	@echo ok

elfx::
	touch rx_prelude_p5.pm  # Assure it exists.
	../../elf/elf_h -x -o ./elfx -I ../../elf/elf_h_src -e 'use Elf_wo_main' ../STD_blue/IRx1_FromAST2.pm -e '$$*ast2ir_0=$$*ast2ir_1; $$*parser0=undef' ../STD_blue/Parser2.pm nodes.pm analysis.pm emit5.pm -e 'sub f is p5 {"eval(q{use remains_of_Regexp_ModuleA;})"};f();' -e '_inline_p5("require \"rx_prelude_p5.pm\";")' -e elf_main
	../../elf/elf_h rx_prelude_create.p6 > rx_prelude.pm
	./elfx -o rx_prelude_p5.pm -s rx_prelude.pm

check_blue:
	-rm -rf lex  # Reset gimme5.
	ELF_STD_BLUE_RUN=../STD_blue/STD_blue_run ./elfx test_blue.pm > tst_1 2> tst_2
	diff t_baseline.result tst_1


old_create_baseline:
	-mkdir qout
	./remains_of_Regexp_ModuleA.pm --test > qout/test_1 2> qout/test_2
	./remains_of_Regexp_ModuleA.pm --test > qout/test_12 2>&1
	./remains_of_Regexp_ModuleA.pm --test6 > qout/test6_1 2> qout/test6_2
	./remains_of_Regexp_ModuleA.pm --test6 > qout/test6_12 2>&1
	-prove t/re_tests.t > qout/prove_re_1 2> qout/prove_re_2
	-prove t/rx.t > qout/prove_rx_1 2> qout/prove_rx_2

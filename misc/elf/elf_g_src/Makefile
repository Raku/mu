
RUN_TESTS=../../elfish/run-tests/run-tests
CWD=$(shell pwd)
test:
	${RUN_TESTS} ${CWD}/../elf_g > test_result
	diff run-tests.result test_result


rebuild: have_parser_cache bootstrap if_bootstrap_succeeded

if_bootstrap_succeeded:
	cp ../elf_g2 ../elf_g

bootstrap:
	# old compiler, new code
	../elf_g -x -o ../elf_g0 Elf.pm
	chmod +x ../elf_g0
	# hybrid compiler
	../elf_g0 -x -o ../elf_g1 Elf.pm
	chmod +x ../elf_g1
	# new compiler
	../elf_g1 -x -o ../elf_g2 Elf.pm
	chmod +x ../elf_g2
	# compiled itself?
	diff ../elf_g1 ../elf_g2

have_parser_cache:
	# assure STD_RED_CACHEDIR is defined
	perl -we 'if(!-d shift){print "undefined!\n";exit(1)}' $(STD_RED_CACHEDIR)

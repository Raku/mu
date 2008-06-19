
RUN_TESTS=../../elfish/run-tests/run-tests
CWD=$(shell pwd)
test:
	${RUN_TESTS} ${CWD}/../elf_f > test_result
	diff run-tests.result test_result


rebuild: have_parser_cache bootstrap if_bootstrap_succeeded

if_bootstrap_succeeded:
	cp ../elf_f2 ../elf_f

bootstrap:
	# old compiler, new code
	../elf_f -x -o ../elf_f0 Elf.pm
	chmod +x ../elf_f0
	# hybrid compiler
	../elf_f0 -x -o ../elf_f1 Elf.pm
	chmod +x ../elf_f1
	# new compiler
	../elf_f1 -x -o ../elf_f2 Elf.pm
	chmod +x ../elf_f2
	# compiled itself?
	diff ../elf_f1 ../elf_f2

have_parser_cache:
	# assure STD_RED_CACHEDIR is defined
	perl -we 'if(!-d shift){print "undefined!\n";exit(1)}' $(STD_RED_CACHEDIR)

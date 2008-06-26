
RUN_TESTS=elfish/run-tests/run-tests
CWD=$(shell pwd)
test:
	${RUN_TESTS} ${CWD}/elfX > test_result
	diff run-tests.result test_result


rebuild: have_parser_cache bootstrap if_bootstrap_succeeded

if_bootstrap_succeeded:
	cp elfX2 elfX

bootstrap:
	# old compiler, new code
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ./elfX -x -o elfX0 Elf.pm
	chmod +x elfX0
	# hybrid compiler
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ./elfX0 -x -o elfX1 Elf.pm
	chmod +x elfX1
	# new compiler
	ELF_STD_RED_RUN=../../STD_red/STD_red_run ./elfX1 -x -o elfX2 Elf.pm
	chmod +x elfX2
	# compiled itself?
	diff elfX1 elfX2

have_parser_cache:
	# assure STD_RED_CACHEDIR is defined
	perl -we 'if(!-d shift){print "undefined!\n";exit(1)}' $(STD_RED_CACHEDIR)

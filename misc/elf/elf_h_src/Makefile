
RUN_TESTS=../../elfish/run-tests/run-tests
CWD=$(shell pwd)

test: bootstrap regression_test

regression_test:: check_baseline_version have_parser_cache
	${RUN_TESTS} ${CWD}/../elf_g > test_result
	diff run-tests.result test_result

test_ok_files: check_baseline_version have_parser_cache
	perl -wne 'next if !/^ok/; /(\.\..*)/ or die; print "$$1\n";' < run-tests.result | xargs prove --exec ../elf_g

check_baseline_version:
	@perl -we '($$a)=`head run-tests.result`=~/(\d+)/;($$b)=`svn info ../../../t`=~/Rev: (\d+)/;if($$a!=$$b){warn("WARNING: svn versions of run-tests.result and t/ dont match ($$a vs $$b).\n")}'


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
	@perl -we 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)

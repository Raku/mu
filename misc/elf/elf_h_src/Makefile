
RUN_TESTS=../../elfish/run-tests/run-tests
CWD=$(shell pwd)

test:: bootstrap_test
	# test no longer runs regression_test.

bootstrap_test:: bootstrap

regression_test:: check_baseline_version have_parser_cache
	# <pugs>/t should be at the revision given in run-tests.result . 
	${RUN_TESTS} ${CWD}/../elf_h > test_result
	diff run-tests.result test_result

test_ok_files: check_baseline_version have_parser_cache
	perl -wne 'next if !/^ok/; /(\.\..*)/ or die; print "$$1\n";' < run-tests.result | xargs prove --exec ../elf_h

check_baseline_version:
	@perl -we '($$a)=`head run-tests.result`=~/(\d+)/;($$b)=`svn info ../../../t`=~/Rev: (\d+)/;if($$a!=$$b){warn("WARNING: svn versions of run-tests.result and t/ dont match ($$a vs $$b).\n")}'


rebuild: have_parser_cache bootstrap if_bootstrap_succeeded

if_bootstrap_succeeded:
	cp ../elf_h2 ../elf_h

bootstrap:
	# old compiler, new code
	../elf_h -x -o ../elf_h0 Elf.pm
	chmod +x ../elf_h0
	# hybrid compiler
	../elf_h0 -x -o ../elf_h1 Elf.pm
	chmod +x ../elf_h1
	# new compiler
	../elf_h1 -x -o ../elf_h2 Elf.pm
	chmod +x ../elf_h2
	# compiled itself?
	diff ../elf_h1 ../elf_h2

have_parser_cache:
	@perl -e 'if(!-d shift){print STDERR "\nDefine STD_RED_CACHEDIR for faster compiles.\n\n";}' $(STD_RED_CACHEDIR)

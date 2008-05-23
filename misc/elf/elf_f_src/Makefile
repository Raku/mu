
test: test_faster bootstrap_faster

test_faster:
	# you might have wanted  test_faster2  instead?
	./run-tests elf_f_faster > test_result_faster
	diff run-tests.result_faster test_result_faster

test_faster2:
	./run-tests elf_f_faster2 > test_result_faster
	diff run-tests.result_faster test_result_faster

test_simple:
	./run-tests elf_f > test_result_simple
	diff run-tests.result_simple test_result_simple


rebuild: rebuild_on_faster
	# Done.

rebuild_on_faster: bootstrap_prep bootstrap_faster bootstrap_simple_w_faster rebuild_succeeded

rebuild_succeeded:
	cp ../elf_f_faster2 ../elf_f_faster
	cp ../elf_f2 ../elf_f

bootstrap_faster:
	# Checking bootstrap of elf_f_faster.
	# old compiler, new code
	../elf_f_faster  -x -o ../elf_f_faster0 Elf_Faster.pm
	chmod +x ../elf_f_faster0
	# hybrid compiler
	../elf_f_faster0 -x -o ../elf_f_faster1 Elf_Faster.pm
	chmod +x ../elf_f_faster1
	# new compiler
	../elf_f_faster1 -x -o ../elf_f_faster2 Elf_Faster.pm
	chmod +x ../elf_f_faster2
	# compiled itself?
	diff ../elf_f_faster1 ../elf_f_faster2

bootstrap_simple_w_faster:
	# Checking bootstrap of elf_f on elf_f_faster.
	# old compiler
	../elf_f_faster -x -o ../elf_f0 Elf.pm
	# hybrid compiler
	../elf_f0 -x -o ../elf_f1 Elf.pm
	# new compiler
	../elf_f1 -x -o ../elf_f2 Elf.pm
	# compiled itself?
	diff ../elf_f1 ../elf_f2

bootstrap_prep:
	# file permissions
	touch ../elf_f ../elf_f0 ../elf_f1 ../elf_f2
	touch ../elf_f_faster ../elf_f_faster0 ../elf_f_faster1 ../elf_f_faster2
	chmod a+x ../elf_f ../elf_f0 ../elf_f1 ../elf_f2
	chmod a+x ../elf_f_faster ../elf_f_faster0 ../elf_f_faster1 ../elf_f_faster2
	# assure STD_RED_CACHEDIR is defined
	perl -we 'if(!-d shift){print "undefined!\n";exit(1)}' $(STD_RED_CACHEDIR)

# extra:
bootstrap_on_simple:
	../elf_f -x -o ../elf_f0 Elf.pm
	chmod +x ../elf_f0
	../elf_f0 -x -o ../elf_f1 Elf.pm
	chmod +x ../elf_f1
	../elf_f1 -x -o ../elf_f2 Elf.pm
	chmod +x ../elf_f2
	diff ../elf_f1 ../elf_f2
	../elf_f2 -e 'say 3'
	#
	../elf_f -x -o ../elf_f_faster0 Elf_Faster.pm
	../elf_f_faster0 -x -o ../elf_f_faster1 Elf_Faster.pm
	../elf_f_faster1 -x -o ../elf_f_faster2 Elf_Faster.pm
	diff ../elf_f_faster1 ../elf_f_faster2
	../elf_f_faster2 -e 'say 3'
	#
	cp ../elf_f_faster2 ../elf_f_faster
	cp ../elf_f2 ../elf_f

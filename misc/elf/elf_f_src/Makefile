
bootstrap: bootstrap_on_faster
	# Done.

bootstrap_on_faster: bootstrap_executable_permissions bootstrap_faster bootstrap_simple_w_faster bootstrap_succeeded

bootstrap_faster:
	# old compiler, new code
	../elf_f_faster  -x -o ../elf_f_faster0 Elf_Faster.pm
	# hybrid compiler
	../elf_f_faster0 -x -o ../elf_f_faster1 Elf_Faster.pm
	# new compiler
	../elf_f_faster1 -x -o ../elf_f_faster2 Elf_Faster.pm
	# compiled itself?
	diff ../elf_f_faster1 ../elf_f_faster2

bootstrap_simple_w_faster:
	# old compiler
	../elf_f_faster -x -o ../elf_f0 Elf.pm
	# hybrid compiler
	../elf_f0 -x -o ../elf_f1 Elf.pm
	# new compiler
	../elf_f1 -x -o ../elf_f2 Elf.pm
	# compiled itself?
	diff ../elf_f1 ../elf_f2

bootstrap_succeeded:
	cp ../elf_f_faster2 ../elf_f_faster
	cp ../elf_f2 ../elf_f

bootstrap_executable_permissions:  ../elf_f2 ../elf_f_faster2 
	touch ../elf_f ../elf_f0 ../elf_f1 ../elf_f2
	touch ../elf_f_faster ../elf_f_faster0 ../elf_f_faster1 ../elf_f_faster2
	chmod a+x ../elf_f ../elf_f0 ../elf_f1 ../elf_f2
	chmod a+x ../elf_f_faster ../elf_f_faster0 ../elf_f_faster1 ../elf_f_faster2

bootstrap_on_simple:
	../elf_f -x -o ../elf_f0 Elf.pm
	../elf_f0 -x -o ../elf_f1 Elf.pm
	../elf_f1 -x -o ../elf_f2 Elf.pm
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


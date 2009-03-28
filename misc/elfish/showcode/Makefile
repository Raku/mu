
P6=../../elf/elf_h
export ELF_STD_RED_RUN := ../../STD_red/STD_red_run

showcode: Makefile ShowCode.pm
	$(P6) -x -o showcode -I ../../elf/elf_h_src -e 'use Elf_wo_main' ShowCode.pm
	chmod a+x showcode

demo: showcode
	./showcode ShowCode.pm > tmp.html
	firefox tmp.html


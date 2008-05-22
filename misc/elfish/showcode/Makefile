
P6=../../elf/elf_f_faster
ElfSrc=../../elf/elf_f_src/
ElfOriginal=$(ElfSrc)/Elf_Faster.pm
ELF_STD_RED_RUN=../../STD_red/STD_red_run

showcode: check_path_to_STD_red_run Makefile elf_core.p5 ShowCode.pm
	cp elf_core.p5 showcode
	$(P6) -s -o deleteme_code ShowCode.pm
	cat deleteme_code >> showcode
	rm deleteme_code
	chmod a+x showcode

elf_core.p5: $(P6)
	grep -v CommandLine $(ElfOriginal) > ElfCore.pm
	$(P6) -x -o elf_core.p5 -I $(ElfSrc) ElfCore.pm

check_path_to_STD_red_run:
	perl -we 'exit(1) if !-f shift;' $(ELF_STD_RED_RUN)
	$(ELF_STD_RED_RUN) -e say 3 > /dev/null

demo: showcode
	./showcode ShowCode.pm > tmp.html
	firefox tmp.html


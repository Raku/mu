use strict;
#use Inline 'INFO','NOISY','FORCE';
#use Inline 'INFO';
use Inline C=>'Config',
	INC => `pkg-config --cflags parrot`,
	LIBS => `pkg-config --libs parrot`,
	TYPEMAPS=>'typemap';
use Inline C=><<'C';
#include "parrot/embed.h"
void greet() {
    printf("Hello, Wonderfull World\n");
}
void run_bytecode(Parrot_Interp i,char *bcfile) {
    Parrot_PackFile pf = Parrot_readbc(i,bcfile);
    Parrot_loadbc(i, pf);
    Parrot_runcode(i,0,NULL);
}
Parrot_Interp new_interpreter() {
   	Parrot_Interp i;
	i = Parrot_new(NULL);
	return i;
}
PMC global(char *name) {
	return Parrot_find_global()
}
C
print "ok 1 - c code compiled\n";
my $i = new_interpreter();
#greet();
#run_bytecode($i,"ok.pbc");
run_bytecode($i,"namespace.pbc");

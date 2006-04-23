use strict;
package Inline::Parrot;
#use Inline 'INFO','NOISY','FORCE';
use Inline C=>'Config',
	INC => `pkg-config --cflags parrot`,
	LIBS => `pkg-config --libs parrot`,
	TYPEMAPS=>'typemap';
use Inline C=><<'C';
#include "parrot/embed.h"
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
C
our $msg = "not ok\n";
our $data;
print "ok 1 - c code compiled\n";
my $i = new_interpreter();
run_bytecode($i,"loadlib.pbc");
print $msg;
print "Data:$data\n";
1;

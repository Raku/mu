use strict;
#use Inline 'INFO','NOISY','FORCE';
# please help fill in this file
# a method of autogenrating bindings from vtable.h is NEEDED
# please commit - pmurias
package Inline::Parrot::PMC;
use Exporter 'import';
our @EXPORT=qw(get_int);
use Inline C=>'Config',
	INC => `pkg-config --cflags parrot`,
	LIBS => `pkg-config --libs parrot`,
	TYPEMAPS=>'typemap';
use Inline C=><<'C';
#include "parrot/parrot.h"
int get_int(Parrot_Interp i,PMC *pmc) {
	return VTABLE_get_integer(i,pmc);
}
C
1;

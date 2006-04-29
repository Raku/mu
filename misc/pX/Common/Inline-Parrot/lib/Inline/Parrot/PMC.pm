use strict;
use warnings;
#use Inline 'INFO','NOISY','FORCE';
# please help fill in this file
# a method of autogenrating bindings from vtable.h is NEEDED
# please commit - pmurias
package Inline::Parrot::PMC;
print "Inline::Parrot::PMC\n";
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
package PMCPtr;
# fix the typemap to make all those stuff live in Inline::Parrot::PMC
our $interpreter;
use overload '0+' => sub {Inline::Parrot::PMC::get_int($Inline::Parrot::PMC::interpreter,$_[0])};
1;

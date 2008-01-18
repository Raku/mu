#!/usr/bin/perl

use strict;
use warnings;

open my $file, '<', 'include/smop_identifiers.h' or die $!;
open my $decl, '>', 'src/idconst_decl_all.h' or die $!;
open my $init, '>', 'src/idconst_init_all.c' or die $!;
open my $dstr, '>', 'src/idconst_destr_all.c' or die $!;


while (<$file>) {
    chomp;
    next unless /^extern SMOP__Object\* (\S+);.+\/\*\s(.+)\s\*\//;
    print {$decl} "SMOP__Object* $1;\n";
    print {$init} "$1 = SMOP__NATIVE__idconst_create( $2 );\n";
    print {$dstr} "SMOP__NATIVE__idconst_free( $1 );\n";
}

close $file;
close $decl;
close $init;
close $dstr;

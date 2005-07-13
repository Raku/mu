use v6;
use Test;

plan 4;

use_ok('Perl::Compiler::PIL');
use_ok('Perl::Compiler::PIL::Util');
use_ok('Perl::Compiler::CodeGen');
use_ok('Perl::Compiler::CodeGen::NameGen');

# XXX - Needs Perl5 Embed - disable for now
#use_ok('Perl::Compiler::CodeGen::Perl5_Str');

# vim: ft=perl6 :

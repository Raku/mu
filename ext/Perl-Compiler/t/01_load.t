#!/usr/bin/pugs

use v6;
use Test;

plan 4;

use Perl::Compiler::PIL; pass "(dummy instead of broken use_ok)";
use Perl::Compiler::PIL::Util; pass "(dummy instead of broken use_ok)";
use Perl::Compiler::CodeGen; pass "(dummy instead of broken use_ok)";
use Perl::Compiler::CodeGen::NameGen; pass "(dummy instead of broken use_ok)";

# XXX - Needs Perl5 Embed - disable for now
#use_ok('Perl::Compiler::CodeGen::Perl5_Str');

# vim: ft=perl6 :

use strict;
use lib '../../../..', '../../../../inc';
use inc::Module::Install (
    -d '../../../../misc/pX' ? (prefix => '../../../../inc') : ()
);

    name('Pugs-Compiler-Perl6');
all_from('lib/Pugs/Compiler/Perl6.pm');
requires('Pugs::Compiler::Rule' => '0');
requires('Module::Compile' => '0');

recommends('Perl::Tidy' => 0);

&WriteAll;

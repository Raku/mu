use strict;
use lib '../../../..', '../../../../inc';
use inc::Module::Install (
    -d '../../../../misc/pX' ? (prefix => '../../../../inc') : ()
);

    name('v6-pugs');
all_from('lib/v6.pm');
requires('Pugs::Compiler::Rule' => '0.06');
requires('Module::Compile' => '0');

recommends('Perl::Tidy' => 0);

&WriteAll;

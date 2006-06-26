use strict;
use inc::Module::Install;

    name('v6-pugs');
abstract('an experimental Perl 6 implementation');
all_from('lib/v6.pm');
requires('Pugs::Compiler::Rule' => '0.06');
requires('Module::Compile' => '0.16');

recommends('Perl::Tidy' => 0);

&WriteAll;

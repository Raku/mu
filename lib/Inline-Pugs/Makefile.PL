use strict;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name('Inline-Pugs');
version('0.01');
abstract('Use Perl 6 from Perl 5 code');
author('Autrijus Tang <autrijus@autrijus.org>');
license('perl');

WritePugs(5);

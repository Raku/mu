use strict;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name('Perl6-Pugs');
version('6.002008');
abstract('A Perl 6 Implementation ');
author('Autrijus Tang <autrijus@autrijus.org>');
license('perl');

WritePugs(5);

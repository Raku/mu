use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perl::Compiler');
version    ('0.01');
abstract   ('The Perl 6 Compiler');
author     ('Luke Palmer');
license    ('perl');

WritePugs  (6);

use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('POE');
version    ('0.0.1');
abstract   ('Perl Object Environment for Perl 6');
author     ('Ingo Blechschmidt <iblech@web.de>');
license    ('perl');

WritePugs  (6);

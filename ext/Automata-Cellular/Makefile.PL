use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Automata::Cellular');
version    ('0.1');
abstract   ('Widget based web templating engine');
author     ('David Brunton <dbrunton@yahoo.com');
license    ('perl');

WritePugs  (6);

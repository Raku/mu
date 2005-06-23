use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perldoc');
version    ('0.01');
abstract   ('Perl Documentation Tools');
author     ('Sam "mugwump" Vilain');
license    ('perl');

WritePugs  (6);

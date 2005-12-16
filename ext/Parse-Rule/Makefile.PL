use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Parse-Rule');
version    ('0.0.1');
abstract   ('Perl 6 port of the Parrot Grammar Engine');
author     ('Luke Palmer <lrpalmer@gmail.com>');
license    ('perl');

WritePugs  (6);

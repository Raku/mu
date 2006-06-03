use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Getopt::Std');
version    ('0.0.1');
abstract   ('Simple commandline processing for Perl 6');
author     ('Gaal Yahas <gaal@forum2.org>');
license    ('perl');

WritePugs  (6);

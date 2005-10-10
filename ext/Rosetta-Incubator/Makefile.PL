use 5.008001;
use utf8;
use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Rosetta-Incubator');
version    ('0.0.1');
abstract   ('Stage for rebirth of the Rosetta Framework');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

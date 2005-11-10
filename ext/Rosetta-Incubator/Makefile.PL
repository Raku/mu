use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Rosetta-Incubator');
version    ('6.0.0');
abstract   ('Stage for rebirth of the Rosetta Framework');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Rosetta');
version    ('0.724.0'); # 'VERSION_FROM' => 'lib/Rosetta.pm'
abstract   ('Rigorous database portability');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

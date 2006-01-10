use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Rosetta::Engine::Native');
version    ('0.1.0'); # 'VERSION_FROM' => 'lib/Rosetta/Engine/Native.pm'
abstract   ('Self-contained reference implementation of a Rosetta Engine');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Relation');
version    ('0.0.1'); # 'VERSION_FROM' => 'lib/Relation.pm'
abstract   ('Relation Perl 6 extension');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('perl');

WritePugs  (6);

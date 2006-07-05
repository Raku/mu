use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Set::Relation');
version    ('0.1.0'); # 'VERSION_FROM' => 'lib/Set/Relation.pm'
abstract   ('Relation data type for Perl');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('perl');

WritePugs  (6);

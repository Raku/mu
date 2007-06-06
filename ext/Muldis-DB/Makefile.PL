use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('QDRDBMS');
version    ('0.0.0'); # 'VERSION_FROM' => 'lib/QDRDBMS.pm'
abstract   ('Full-featured truly relational DBMS in Perl');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

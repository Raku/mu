use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Muldis::Rosetta');
version    ('0.6.2');
abstract   ('Full-featured truly relational DBMS in Perl');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('lgpl');

WritePugs  (6);

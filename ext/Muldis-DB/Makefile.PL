use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Muldis::DB');
version    ('0.0.1');
abstract   ('Full-featured truly relational DBMS in Perl');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('gpl');

WritePugs  (6);

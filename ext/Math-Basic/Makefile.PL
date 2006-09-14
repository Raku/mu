use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Math::Basic');
version    ('0.01');
abstract   ('Perl 6 Math::Basic (S29)');
author     ('Pugs team');
license    ('perl');

WritePugs  (6);

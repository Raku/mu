use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Test');
version    ('0.01');
abstract   ('Test Module');
author     ('Audrey Tang <autrijus@cpan.org>');
license    ('perl');

WritePugs  (6);

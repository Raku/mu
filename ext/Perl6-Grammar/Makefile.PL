use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perl6-Grammar');
version    ('0.0.1');
abstract   ('Perl 6 Grammar');
author     ('Mitchell N "putter" Charity');
license    ('perl');

WritePugs  (6);

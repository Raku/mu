use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Perl6::Value::List');
version    ('0.01');
abstract   ('Implementation of Perl6 "List"');
author     ('Flavio S. Glock');
license    ('perl');

WritePugs  (6);

use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Array::Lazy');
version    ('0.01');
abstract   ('Implementation of lazy arrays');
author     ('Flavio S. Glock');
license    ('perl');

WritePugs  (6);

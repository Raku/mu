use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Benchmark');
version    ('0.1');
abstract   ('Benchmark for Pugs');
author     ('clkao');
license    ('perl');

WritePugs  (6);

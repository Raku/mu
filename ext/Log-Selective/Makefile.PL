use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Log::Selective');
version    ('0.01');
abstract   ('Selectively choose what to log');
author     ('Brent Royal-Gordon <brent@brentdax.com>');
license    ('perl');

WritePugs  (6);

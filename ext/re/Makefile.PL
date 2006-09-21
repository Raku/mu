use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('re');
version    ('0.01');
abstract   ('selector for RE engine');
author     ('The Pugs Team <perl6-compiler@perl.org>');
license    ('perl');

WritePugs  (6);


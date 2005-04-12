use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('HTML::Entities');
version    ('0.01');
abstract   ('HTML::Entities for Pugs');
author     ('Curtis "Ovid" Poe <publiustemp-No-pugs-SPAM at yahoo com');
license    ('perl');

WritePugs  (6);

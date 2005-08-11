use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('WTemplate');
version    ('0.01');
abstract   ('Widget based web templating engine');
author     ('Andras Barthazi <andras.barthazi@wish.hu>');
license    ('perl');

WritePugs  (6);

use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Language-AttributeGrammar');
version    ('0.0.1');
abstract   ('Attribute grammars for computations over trees';
author     ('Luke Palmer <lrpalmer@gmail.com>');
license    ('perl');

WritePugs  (6);

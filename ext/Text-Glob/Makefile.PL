use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Text::Glob');
version    ('6.00');
abstract   ('Translate glob patterns to regexes');
author     ('Sebastian Riedel <sri@oook.de>');
license    ('perl');

WritePugs  (6);

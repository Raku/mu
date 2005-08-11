use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('FindBin');
version    ('6.00');
abstract   ('Locate directory of original perl script');
author     ('Sebastian Riedel <sri@oook.de>');
license    ('perl');

WritePugs  (6);

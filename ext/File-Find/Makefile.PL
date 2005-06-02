use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('File::Find');
version    ('6.00');
abstract   ('Traverse a directory tree');
author     ('Sebastian Riedel <sri@oook.de>');
license    ('perl');

WritePugs  (6);

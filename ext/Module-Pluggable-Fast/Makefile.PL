use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Module::Pluggable::Fast');
version    ('6.00');
abstract   ('Fast plugins');
author     ('Sebastian Riedel <sri@oook.de>');
license    ('perl');

WritePugs  (6);

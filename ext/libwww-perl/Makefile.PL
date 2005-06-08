use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('libwww-perl');
version    ('0.0.1');
abstract   ('Simple procedural interface to LWP');
author     ('Max Maischein <corion@cpan.org>');
license    ('perl');

WritePugs  (6);

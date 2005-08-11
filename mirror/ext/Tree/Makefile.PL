use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Tree');
version    ('0.01');
abstract   ('A simple n-ary Tree');
author     ('Stevan Little <stevan@iinteractive.com>');
license    ('perl');

WritePugs  (6);

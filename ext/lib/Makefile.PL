use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('lib');
version    ('0.01');
abstract   ('Add paths to @*INC');
author     ('Stevan Little <stevan@iinteractive.com>');
license    ('perl');

WritePugs  (6);

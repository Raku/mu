use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('FileSpec');
version    ('0.01');
abstract   ('Module for portable file handling');
author     ('Stevan Little <stevan@iinteractive.com>');
license    ('perl');

WritePugs  (6);

use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Pod::Event::Parser');
version    ('0.02');
abstract   ('Event based POD parser');
author     ('Stevan Little <stevan@iinteractive.com>');
license    ('perl');

WritePugs  (6);

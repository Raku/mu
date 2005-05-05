use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Pod::Stream::Parser');
version    ('0.01');
abstract   ('Stream/Event based POD parser');
author     ('Stevan Little <stevan@iinteractive.com>');
license    ('perl');

WritePugs  (6);

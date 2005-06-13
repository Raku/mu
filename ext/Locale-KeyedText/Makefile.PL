use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Locale-KeyedText');
version    ('0.1.2'); # based on 5v1.05; to become 6v1.5.0 when fully functional
abstract   ('Refer to user messages in programs by keys');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('lgpl');

WritePugs  (6);

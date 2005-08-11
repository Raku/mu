use strict;
use lib "../..", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Locale-KeyedText');
version    ('1.5.0'); # based on 5v1.05
abstract   ('Refer to user messages in programs by keys');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('lgpl');

WritePugs  (6);

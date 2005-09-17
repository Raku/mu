use strict;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Locale-KeyedText');
version    ('1.6.2'); # based on 5v1.6.2
abstract   ('Refer to user messages in programs by keys');
author     ('Darren Duncan <perl@DarrenDuncan.net>');
license    ('lgpl');

WritePugs  (6);

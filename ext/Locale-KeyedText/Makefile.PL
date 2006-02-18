use strict;
use warnings;

use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

name       ('Locale::KeyedText');
version    ('1.72.2'); # 'VERSION_FROM' => 'lib/Locale/KeyedText.pm'
abstract   ('Refer to user messages in programs by keys');
author     ('Darren R. Duncan <perl@DarrenDuncan.net>');
license    ('lgpl');

WritePugs  (6);
